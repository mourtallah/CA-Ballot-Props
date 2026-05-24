import { query } from '../db/client';
import type { Post, PostRow, FanoutStrategy } from '../types';

function rowToPost(row: PostRow): Post {
  const post: Post = {
    id: row.id, authorId: row.author_id, content: row.content,
    replyToId: row.reply_to_id, likeCount: row.like_count,
    replyCount: row.reply_count, repostCount: row.repost_count,
    fanoutComplete: row.fanout_complete, fanoutStrategy: row.fanout_strategy,
    authorFollowersAtPost: row.author_followers_at_post, createdAt: row.created_at,
  };
  if (row.author_username) {
    post.author = {
      id: row.author_id, username: row.author_username,
      displayName: row.author_display_name ?? '', avatarUrl: row.author_avatar_url,
      followerCount: row.author_follower_count ?? 0,
    };
  }
  return post;
}

export async function createPost(data: {
  authorId: string; content: string; replyToId?: string;
  fanoutStrategy: FanoutStrategy; authorFollowersAtPost: number;
}): Promise<Post> {
  const rows = await query<PostRow>(
    `INSERT INTO posts (author_id,content,reply_to_id,fanout_strategy,author_followers_at_post)
     VALUES ($1,$2,$3,$4,$5) RETURNING *`,
    [data.authorId, data.content, data.replyToId ?? null, data.fanoutStrategy, data.authorFollowersAtPost]
  );
  return rowToPost(rows[0]);
}

export async function findById(postId: string): Promise<Post | null> {
  const rows = await query<PostRow>(
    `SELECT p.*,u.username AS author_username,u.display_name AS author_display_name,
     u.avatar_url AS author_avatar_url,u.follower_count AS author_follower_count
     FROM posts p JOIN users u ON u.id=p.author_id WHERE p.id=$1`,
    [postId]
  );
  return rows[0] ? rowToPost(rows[0]) : null;
}

export async function getPostsByAuthor(
  authorId: string, opts: { limit?: number; before?: Date } = {}
): Promise<Post[]> {
  const { limit = 20, before } = opts;
  const params: unknown[] = [authorId, limit];
  if (before) params.push(before);
  const rows = await query<PostRow>(
    `SELECT p.*,u.username AS author_username,u.display_name AS author_display_name,
     u.avatar_url AS author_avatar_url,u.follower_count AS author_follower_count
     FROM posts p JOIN users u ON u.id=p.author_id
     WHERE p.author_id=$1 ${before ? 'AND p.created_at<$3' : ''}
     ORDER BY p.created_at DESC LIMIT $2`,
    params
  );
  return rows.map(rowToPost);
}

export async function markFanoutComplete(postId: string): Promise<void> {
  await query('UPDATE posts SET fanout_complete=TRUE WHERE id=$1', [postId]);
}

export async function getByIds(postIds: string[]): Promise<Post[]> {
  if (!postIds.length) return [];
  const rows = await query<PostRow>(
    `SELECT p.*,u.username AS author_username,u.display_name AS author_display_name,
     u.avatar_url AS author_avatar_url,u.follower_count AS author_follower_count
     FROM posts p JOIN users u ON u.id=p.author_id
     WHERE p.id=ANY($1::uuid[]) ORDER BY p.created_at DESC`,
    [postIds]
  );
  return rows.map(rowToPost);
}
