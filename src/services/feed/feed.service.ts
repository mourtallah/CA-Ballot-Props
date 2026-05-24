/**
 * Feed Assembly Service
 *
 * Assembles a user's feed from two sources:
 *   1. Push inbox (Redis ZSET) — posts pushed by small/hybrid accounts
 *   2. Pull from DB — recent posts by large/celebrity accounts the user follows
 *
 * Merge strategy: Sorted by timestamp descending, deduplicated by postId.
 *
 * For accounts with large-account followees, we do an on-demand DB query
 * for recent posts (last 24h) from those accounts, ranked by engagement.
 */
import { getRedis, Keys } from '../../db/redis';
import * as postModel from '../../models/post.model';
import { query } from '../../db/client';
import { config } from '../../config';
import { logger } from '../../lib/logger';
import { feedRequestDuration } from '../../lib/metrics';
import type { FeedPage, FeedItem, Post, PostRow } from '../../types';

const DEFAULT_PAGE_SIZE = 20;
const MAX_PAGE_SIZE = 50;

// ─── Feed Assembly ────────────────────────────────────────────────────────

export async function assembleFeed(
  userId: string,
  opts: { limit?: number; cursor?: string } = {}
): Promise<FeedPage> {
  const start = Date.now();
  const limit = Math.min(opts.limit ?? DEFAULT_PAGE_SIZE, MAX_PAGE_SIZE);
  const redis = getRedis();

  // Determine cursor as a timestamp (ms)
  const cursorTs = opts.cursor
    ? parseInt(Buffer.from(opts.cursor, 'base64').toString(), 10)
    : Date.now();

  try {
    // ── 1. Pull from Redis inbox (push-path items) ────────────────────────
    // ZSET scored by timestamp. We read items newer than cursor but limit to `limit * 2`
    // to have enough after merging with pull results.
    const inboxItems = await redis.zrevrangebyscore(
      Keys.userInbox(userId),
      cursorTs,
      '-inf',
      'WITHSCORES',
      'LIMIT', 0, limit * 2
    );

    const inboxPostIds: string[] = [];
    const inboxTimestamps: Record<string, number> = {};
    for (let i = 0; i < inboxItems.length; i += 2) {
      inboxPostIds.push(inboxItems[i]);
      inboxTimestamps[inboxItems[i]] = parseInt(inboxItems[i + 1], 10);
    }

    // ── 2. Pull path: recent posts from celebrity/pull-strategy followees ─
    const pullPosts = await fetchPullPosts(userId, cursorTs, limit);

    // ── 3. Own posts (always included) ───────────────────────────────────
    const ownPosts = await postModel.getPostsByAuthor(userId, {
      limit: 5,
      before: new Date(cursorTs),
    });

    // ── 4. Resolve inbox post IDs to full Post objects ────────────────────
    const inboxPosts = await postModel.getByIds(inboxPostIds);

    // ── 5. Merge, deduplicate, sort ───────────────────────────────────────
    const allItems: FeedItem[] = [];
    const seen = new Set<string>();

    for (const post of inboxPosts) {
      if (!seen.has(post.id)) {
        seen.add(post.id);
        allItems.push({ post, source: 'inbox' });
      }
    }

    for (const post of pullPosts) {
      if (!seen.has(post.id)) {
        seen.add(post.id);
        allItems.push({ post, source: 'pull' });
      }
    }

    for (const post of ownPosts) {
      if (!seen.has(post.id)) {
        seen.add(post.id);
        allItems.push({ post, source: 'own' });
      }
    }

    // Sort by post creation time descending
    allItems.sort((a, b) =>
      b.post.createdAt.getTime() - a.post.createdAt.getTime()
    );

    // ── 6. Apply simple engagement ranking (boost for popular posts) ──────
    for (const item of allItems) {
      item.score = scorePost(item.post);
    }
    // Re-sort with score (weighted recency + engagement)
    allItems.sort((a, b) => (b.score ?? 0) - (a.score ?? 0));

    const pageItems = allItems.slice(0, limit);
    const hasMore = allItems.length > limit;

    // Cursor = timestamp of the oldest item in this page
    let nextCursor: string | undefined;
    if (hasMore && pageItems.length > 0) {
      const oldestTs = pageItems[pageItems.length - 1].post.createdAt.getTime();
      nextCursor = Buffer.from(String(oldestTs)).toString('base64');
    }

    feedRequestDuration.observe({ path: 'feed' }, Date.now() - start);

    return { items: pageItems, nextCursor, hasMore };

  } catch (err) {
    logger.error('Feed assembly error', { userId, error: String(err) });
    throw err;
  }
}

// ─── Pull Path ────────────────────────────────────────────────────────────

async function fetchPullPosts(
  userId: string,
  beforeTs: number,
  limit: number
): Promise<Post[]> {
  // Find followees that use pull/hybrid strategy
  const rows = await query<PostRow>(
    `SELECT p.*,
            u.username AS author_username,
            u.display_name AS author_display_name,
            u.avatar_url AS author_avatar_url,
            u.follower_count AS author_follower_count
     FROM posts p
     JOIN follows f ON f.followee_id = p.author_id
     JOIN users u ON u.id = p.author_id
     WHERE f.follower_id = $1
       AND p.created_at < $2
       AND p.fanout_strategy IN ('pull', 'hybrid')
       AND p.fanout_complete = TRUE
     ORDER BY p.created_at DESC
     LIMIT $3`,
    [userId, new Date(beforeTs), limit]
  );

  return rows.map((row) => ({
    id: row.id,
    authorId: row.author_id,
    content: row.content,
    replyToId: row.reply_to_id,
    likeCount: row.like_count,
    replyCount: row.reply_count,
    repostCount: row.repost_count,
    fanoutComplete: row.fanout_complete,
    fanoutStrategy: row.fanout_strategy,
    authorFollowersAtPost: row.author_followers_at_post,
    createdAt: row.created_at,
    author: row.author_username ? {
      id: row.author_id,
      username: row.author_username,
      displayName: row.author_display_name ?? '',
      avatarUrl: row.author_avatar_url,
      followerCount: row.author_follower_count ?? 0,
    } : undefined,
  }));
}

// ─── Ranking ──────────────────────────────────────────────────────────────

/**
 * Simple time-decayed engagement score.
 * score = engagement / (age_hours + 2)^1.5
 * (HackerNews-style but with engagement weight)
 */
function scorePost(post: Post): number {
  const ageMs = Date.now() - post.createdAt.getTime();
  const ageHours = ageMs / (1000 * 60 * 60);
  const engagement = post.likeCount * 1 + post.replyCount * 2 + post.repostCount * 3;
  return (engagement + 1) / Math.pow(ageHours + 2, 1.5);
}
