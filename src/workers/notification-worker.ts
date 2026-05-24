/**
 * Notification Worker
 *
 * Consumes the `notification:queue` Redis Stream.
 * For each notification event:
 *   1. Checks if the recipient is connected to any WS server
 *   2. If connected here: delivers directly via Socket.io
 *   3. If connected elsewhere: publishes a routing message to that server
 *   4. Persists notification to PostgreSQL for offline/unread badge
 */
import { getRedis, Keys, ensureStreamGroup } from '../db/redis';
import { deliverToUser } from '../services/realtime/websocket.service';
import { query } from '../db/client';
import { logger } from '../lib/logger';
import { config } from '../config';
import type { RealtimeEvent, RealtimeEventType } from '../types';

const CONSUMER_GROUP = 'notification-workers';
const CONSUMER_NAME = `notif-worker-${process.pid}`;
const BATCH_SIZE = 50;
const BLOCK_MS = 2_000;

// Monotonic sequence counter (per-process; in production use Redis INCR)
let seqCounter = Date.now();

async function run(): Promise<void> {
  const redis = getRedis();
  const stream = Keys.notificationQueue();

  await ensureStreamGroup(redis, stream, CONSUMER_GROUP);
  logger.info('Notification worker started', { consumer: CONSUMER_NAME });

  while (true) {
    try {
      const results = await redis.xreadgroup(
        'GROUP', CONSUMER_GROUP, CONSUMER_NAME,
        'COUNT', BATCH_SIZE,
        'BLOCK', BLOCK_MS,
        'STREAMS', stream, '>'
      ) as Array<[string, Array<[string, string[]]>]> | null;

      if (!results) continue;

      const [[, messages]] = results;

      for (const [messageId, fields] of messages) {
        const dataJson = fields[1];
        if (!dataJson) {
          await redis.xack(stream, CONSUMER_GROUP, messageId);
          continue;
        }

        const data = JSON.parse(dataJson) as {
          type: RealtimeEventType;
          recipientId: string;
          postId: string;
          authorId: string;
        };

        try {
          await processNotification(data);
          await redis.xack(stream, CONSUMER_GROUP, messageId);
        } catch (err) {
          logger.error('Notification processing error', {
            messageId,
            error: String(err),
          });
        }
      }
    } catch (err) {
      logger.error('Notification worker loop error', { error: String(err) });
      await sleep(500);
    }
  }
}

async function processNotification(data: {
  type: RealtimeEventType;
  recipientId: string;
  postId: string;
  authorId: string;
}): Promise<void> {
  const seq = ++seqCounter;

  const event: RealtimeEvent = {
    type: data.type,
    seq,
    userId: data.recipientId,
    payload: {
      postId: data.postId,
      authorId: data.authorId,
    },
    timestamp: new Date().toISOString(),
  };

  // Try direct delivery (if user is connected to this WS server)
  const delivered = await deliverToUser(event);

  // Persist to DB regardless (for offline/unread badge)
  await query(
    `INSERT INTO notifications (user_id, type, actor_id, post_id, payload, delivered_at)
     VALUES ($1, $2, $3, $4, $5, $6)
     ON CONFLICT DO NOTHING`,
    [
      data.recipientId,
      data.type,
      data.authorId,
      data.postId,
      JSON.stringify(event.payload),
      delivered ? new Date() : null,
    ]
  );
}

function sleep(ms: number): Promise<void> {
  return new Promise(resolve => setTimeout(resolve, ms));
}

process.on('SIGTERM', () => {
  logger.info('Notification worker shutting down');
  process.exit(0);
});

run().catch(err => {
  logger.error('Notification worker crashed', { error: String(err) });
  process.exit(1);
});
