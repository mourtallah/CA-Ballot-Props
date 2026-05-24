/**
 * ╔══════════════════════════════════════════════════════════════════════════╗
 * ║                    HYBRID FAN-OUT SERVICE                                ║
 * ║                                                                          ║
 * ║  The core of the system. Determines and executes the fan-out strategy    ║
 * ║  for each new post based on the author's follower count.                 ║
 * ║                                                                          ║
 * ║  Strategy Matrix:                                                        ║
 * ║  ┌──────────────────┬───────────────────────────────────────────────┐  ║
 * ║  │ followers < 1K   │ PUSH — fan-out to all followers immediately   │  ║
 * ║  │ 1K ≤ f < 10K     │ HYBRID — push to active followers only        │  ║
 * ║  │ followers ≥ 10K  │ PULL — push lightweight hints to active       │  ║
 * ║  └──────────────────┴───────────────────────────────────────────────┘  ║
 * ╚══════════════════════════════════════════════════════════════════════════╝
 */
import { getRedis, Keys } from '../../db/redis';
import * as userModel from '../../models/user.model';
import * as postModel from '../../models/post.model';
import { config } from '../../config';
import { logger } from '../../lib/logger';
import {
  fanoutTotal,
  fanoutDuration,
  fanoutTargetSize,
  postsCreatedTotal,
} from '../../lib/metrics';
import type { FanoutJob, FanoutResult, FanoutStrategy, Post } from '../../types';
import { query } from '../../db/client';

const BATCH_SIZE = 500;   // Process follower IDs in batches to avoid memory bloat

// ─── Strategy Resolution ──────────────────────────────────────────────────

export function resolveStrategy(followerCount: number): FanoutStrategy {
  const { threshold, hybridThreshold } = config.fanout;
  if (followerCount >= threshold) return 'pull';
  if (followerCount >= hybridThreshold) return 'hybrid';
  return 'push';
}

// ─── Main Dispatch ────────────────────────────────────────────────────────

/**
 * Called synchronously during post creation.
 * For small accounts: runs immediately (or enqueues for async).
 * For large accounts: enqueues a lightweight hint job.
 */
export async function dispatchFanout(
  post: Post,
  authorFollowerCount: number
): Promise<void> {
  const strategy = resolveStrategy(authorFollowerCount);
  const redis = getRedis();

  logger.info('Dispatching fan-out', {
    postId: post.id,
    authorId: post.authorId,
    followerCount: authorFollowerCount,
    strategy,
  });

  postsCreatedTotal.inc({ fanout_strategy: strategy });

  // Enqueue fan-out job to Redis Stream for async processing
  const job: FanoutJob = {
    postId: post.id,
    authorId: post.authorId,
    authorFollowerCount,
    strategy,
    createdAt: new Date().toISOString(),
  };

  await redis.xadd(
    Keys.fanoutQueue(),
    '*',
    'job', JSON.stringify(job)
  );
}

// ─── Fan-out Executors ────────────────────────────────────────────────────

/**
 * PUSH strategy: write post reference to every follower's inbox.
 * Time complexity: O(followerCount)
 * Suitable for accounts with < 1K followers.
 */
export async function executePushFanout(job: FanoutJob): Promise<FanoutResult> {
  const start = Date.now();
  const redis = getRedis();

  let processed = 0;
  let failed = 0;
  let offset = 0;
  const timestamp = Date.now();

  try {
    while (true) {
      // Fetch follower IDs in batches to avoid loading 1M IDs into memory
      const followerIds = await userModel.getFollowerIds(job.authorId, {
        limit: BATCH_SIZE,
        offset,
      });

      if (followerIds.length === 0) break;

      // Pipeline all inbox writes for this batch
      const pipeline = redis.pipeline();
      for (const followerId of followerIds) {
        pipeline.zadd(
          Keys.userInbox(followerId),
          timestamp,
          job.postId
        );
        // Trim inbox to keep it bounded (newest 1000 items)
        pipeline.zremrangebyrank(Keys.userInbox(followerId), 0, -1001);
        // Set TTL
        pipeline.expire(Keys.userInbox(followerId), config.fanout.inboxTtlSeconds);
      }
      await pipeline.exec();

      // Also publish to notification queue for real-time delivery
      const notifPipeline = redis.pipeline();
      for (const followerId of followerIds) {
        notifPipeline.xadd(
          Keys.notificationQueue(),
          '*',
          'data', JSON.stringify({
            type: 'new_post',
            recipientId: followerId,
            postId: job.postId,
            authorId: job.authorId,
          })
        );
      }
      await notifPipeline.exec();

      processed += followerIds.length;
      offset += BATCH_SIZE;

      if (followerIds.length < BATCH_SIZE) break;  // Last batch
    }

    await postModel.markFanoutComplete(job.postId);
    fanoutTotal.inc({ strategy: 'push' });

  } catch (err) {
    failed++;
    logger.error('Push fan-out error', { postId: job.postId, error: String(err) });
  }

  const durationMs = Date.now() - start;
  fanoutDuration.observe({ strategy: 'push' }, durationMs);
  fanoutTargetSize.observe({ strategy: 'push' }, processed);

  logger.info('Push fan-out complete', {
    postId: job.postId,
    processed,
    durationMs,
  });

  return { postId: job.postId, strategy: 'push', totalTargets: processed + failed, processed, failed, durationMs };
}

/**
 * HYBRID strategy: push only to *active* followers (logged in within 24h).
 * Passive followers will pull on next open.
 * Reduces write amplification by ~60-80% for mid-tier accounts.
 */
export async function executeHybridFanout(job: FanoutJob): Promise<FanoutResult> {
  const start = Date.now();
  const redis = getRedis();
  const timestamp = Date.now();
  let processed = 0;
  let failed = 0;
  let offset = 0;

  try {
    while (true) {
      const followerIds = await userModel.getFollowerIds(job.authorId, {
        limit: BATCH_SIZE,
        activeOnly: true,   // KEY DIFFERENCE: only active followers
        offset,
      });

      if (followerIds.length === 0) break;

      const pipeline = redis.pipeline();
      for (const followerId of followerIds) {
        pipeline.zadd(Keys.userInbox(followerId), timestamp, job.postId);
        pipeline.zremrangebyrank(Keys.userInbox(followerId), 0, -1001);
        pipeline.expire(Keys.userInbox(followerId), config.fanout.inboxTtlSeconds);
      }
      await pipeline.exec();

      // Notify active followers in real-time
      const notifPipeline = redis.pipeline();
      for (const followerId of followerIds) {
        notifPipeline.xadd(
          Keys.notificationQueue(),
          '*',
          'data', JSON.stringify({
            type: 'new_post',
            recipientId: followerId,
            postId: job.postId,
            authorId: job.authorId,
          })
        );
      }
      await notifPipeline.exec();

      processed += followerIds.length;
      offset += BATCH_SIZE;
      if (followerIds.length < BATCH_SIZE) break;
    }

    await postModel.markFanoutComplete(job.postId);
    fanoutTotal.inc({ strategy: 'hybrid' });

  } catch (err) {
    failed++;
    logger.error('Hybrid fan-out error', { postId: job.postId, error: String(err) });
  }

  const durationMs = Date.now() - start;
  fanoutDuration.observe({ strategy: 'hybrid' }, durationMs);
  fanoutTargetSize.observe({ strategy: 'hybrid' }, processed);

  return { postId: job.postId, strategy: 'hybrid', totalTargets: processed + failed, processed, failed, durationMs };
}

/**
 * PULL strategy: for celebrity/viral accounts (10K+ followers).
 * We do NOT write to millions of inboxes.
 * Instead: push a lightweight "new content hint" to a small fraction
 * of the most active followers. Everyone else fetches on next open.
 *
 * Blast radius: O(followerCount × hintFraction) instead of O(followerCount)
 * Example: 1M followers × 0.1 = 100K hint deliveries instead of 1M inbox writes
 */
export async function executePullFanout(job: FanoutJob): Promise<FanoutResult> {
  const start = Date.now();
  const redis = getRedis();
  const timestamp = Date.now();
  let processed = 0;
  let failed = 0;

  try {
    if (config.features.celebrityHints) {
      // Push lightweight hints to a fraction of active followers
      const maxHintTargets = Math.floor(
        job.authorFollowerCount * config.fanout.hintFraction
      );
      const hintLimit = Math.min(maxHintTargets, 50_000);  // Safety cap

      const activeFollowerIds = await userModel.getFollowerIds(job.authorId, {
        limit: hintLimit,
        activeOnly: true,
      });

      if (activeFollowerIds.length > 0) {
        // Store hints in per-user sorted set (client fetches on demand)
        const pipeline = redis.pipeline();
        for (const followerId of activeFollowerIds) {
          pipeline.zadd(Keys.celebrityHints(followerId), timestamp, job.postId);
          pipeline.expire(Keys.celebrityHints(followerId), config.fanout.inboxTtlSeconds);
          // Push real-time hint (lightweight — no content, just "go fetch")
          pipeline.xadd(
            Keys.notificationQueue(),
            '*',
            'data', JSON.stringify({
              type: 'hint',
              recipientId: followerId,
              postId: job.postId,
              authorId: job.authorId,
            })
          );
        }
        await pipeline.exec();
        processed = activeFollowerIds.length;
      }
    }

    // Mark post as available for pull — no inbox writes for the rest
    await postModel.markFanoutComplete(job.postId);
    fanoutTotal.inc({ strategy: 'pull' });

    logger.info('Pull fan-out (celebrity) complete', {
      postId: job.postId,
      totalFollowers: job.authorFollowerCount,
      hintsDelivered: processed,
      hintFraction: config.fanout.hintFraction,
    });

  } catch (err) {
    failed++;
    logger.error('Pull fan-out error', { postId: job.postId, error: String(err) });
  }

  const durationMs = Date.now() - start;
  fanoutDuration.observe({ strategy: 'pull' }, durationMs);
  fanoutTargetSize.observe({ strategy: 'pull' }, processed);

  return { postId: job.postId, strategy: 'pull', totalTargets: job.authorFollowerCount, processed, failed, durationMs };
}

/**
 * Route job to the correct executor based on strategy.
 */
export async function executeFanout(job: FanoutJob): Promise<FanoutResult> {
  // Record event in DB for observability
  const [event] = await query<{ id: string }>(
    `INSERT INTO fanout_events (post_id, strategy, total_targets, status)
     VALUES ($1, $2, $3, 'running')
     RETURNING id`,
    [job.postId, job.strategy, job.authorFollowerCount]
  );
  const eventId = event?.id;

  try {
    let result: FanoutResult;
    switch (job.strategy) {
      case 'push':   result = await executePushFanout(job);   break;
      case 'hybrid': result = await executeHybridFanout(job); break;
      case 'pull':   result = await executePullFanout(job);   break;
    }

    if (eventId) {
      await query(
        `UPDATE fanout_events
         SET status = 'done', processed = $2, failed = $3,
             duration_ms = $4, completed_at = NOW()
         WHERE id = $1`,
        [eventId, result.processed, result.failed, result.durationMs]
      );
    }

    return result;
  } catch (err) {
    if (eventId) {
      await query(
        `UPDATE fanout_events SET status = 'failed', error_message = $2 WHERE id = $1`,
        [eventId, String(err)]
      );
    }
    throw err;
  }
}
