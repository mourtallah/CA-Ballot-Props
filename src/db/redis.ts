/**
 * Redis client + key naming conventions
 *
 * Key schema:
 *   user:inbox:{userId}        → ZSET  (score=timestamp, value=postId)
 *   user:seq:{userId}          → STRING (last delivered seq number)
 *   user:connections:{userId}  → SET   (ws-server IDs where user is connected)
 *   ws:server:{serverId}:users → SET   (userIds connected to this WS server)
 *   replay:{userId}            → STREAM (missed events for replay)
 *   fanout:queue               → STREAM (fan-out jobs)
 *   notification:queue         → STREAM (notification delivery jobs)
 *   celebrity:hints:{userId}   → ZSET  (pending hint post IDs)
 *   active:users               → ZSET  (score=last-seen-ts)
 */
import Redis from 'ioredis';
import { config } from '../config';
import { logger } from '../lib/logger';

let redisClient: Redis | null = null;

export function getRedis(): Redis {
  if (!redisClient) {
    redisClient = new Redis({
      host: config.redis.host,
      port: config.redis.port,
      password: config.redis.password || undefined,
      db: config.redis.db,
      maxRetriesPerRequest: 3,
      enableReadyCheck: true,
    });
    redisClient.on('error', (err) => logger.error('Redis error', { error: err.message }));
    redisClient.on('connect', () => logger.info('Redis connected'));
  }
  return redisClient;
}

export const Keys = {
  userInbox:         (userId: string) => `user:inbox:${userId}`,
  userSeq:           (userId: string) => `user:seq:${userId}`,
  userConnections:   (userId: string) => `user:connections:${userId}`,
  wsServerUsers:     (serverId: string) => `ws:server:${serverId}:users`,
  replayBuffer:      (userId: string) => `replay:${userId}`,
  fanoutQueue:       () => 'fanout:queue',
  notificationQueue: () => 'notification:queue',
  celebrityHints:    (userId: string) => `celebrity:hints:${userId}`,
  rateFanout:        (postId: string) => `rate:fanout:${postId}`,
  activeUsers:       () => 'active:users',
};

export async function ensureStreamGroup(
  redis: Redis, stream: string, group: string
): Promise<void> {
  try {
    await redis.xgroup('CREATE', stream, group, '$', 'MKSTREAM');
  } catch (err: unknown) {
    if (!(err instanceof Error) || !err.message.includes('BUSYGROUP')) throw err;
  }
}

export async function closeRedis(): Promise<void> {
  if (redisClient) { await redisClient.quit(); redisClient = null; }
}
