/**
 * Fan-out Worker
 *
 * Consumes the `fanout:queue` Redis Stream and executes fan-out jobs.
 * Runs as a separate process; multiple instances can run in parallel
 * for different consumer group members.
 *
 * Uses consumer groups for at-least-once delivery semantics.
 * On crash, unACK'd messages are reclaimed after a timeout.
 */
import { getRedis, Keys, ensureStreamGroup } from '../db/redis';
import { executeFanout } from '../services/fanout/fanout.service';
import { logger } from '../lib/logger';
import { config } from '../config';
import type { FanoutJob } from '../types';

const CONSUMER_GROUP = 'fanout-workers';
const CONSUMER_NAME = `worker-${process.pid}-${config.wsServerId}`;
const BATCH_SIZE = 10;
const BLOCK_MS = 5_000;   // Block waiting for new messages
const CLAIM_IDLE_MS = 60_000;  // Reclaim messages idle for 60s

async function run(): Promise<void> {
  const redis = getRedis();
  const stream = Keys.fanoutQueue();

  await ensureStreamGroup(redis, stream, CONSUMER_GROUP);
  logger.info('Fan-out worker started', { consumer: CONSUMER_NAME });

  // Periodically reclaim stuck messages from dead workers
  setInterval(() => reclaimStuck(redis, stream), 30_000);

  while (true) {
    try {
      // Read up to BATCH_SIZE messages from our consumer group
      const results = await redis.xreadgroup(
        'GROUP', CONSUMER_GROUP, CONSUMER_NAME,
        'COUNT', BATCH_SIZE,
        'BLOCK', BLOCK_MS,
        'STREAMS', stream, '>'
      ) as Array<[string, Array<[string, string[]]>]> | null;

      if (!results || results.length === 0) continue;

      const [[, messages]] = results;

      for (const [messageId, fields] of messages) {
        const jobJson = fields[1]; // fields = ['job', jsonString]
        if (!jobJson) {
          await redis.xack(stream, CONSUMER_GROUP, messageId);
          continue;
        }

        const job: FanoutJob = { ...JSON.parse(jobJson), messageId };

        try {
          await executeFanout(job);
          await redis.xack(stream, CONSUMER_GROUP, messageId);
          logger.debug('Fan-out job ACK', { messageId, postId: job.postId });
        } catch (err) {
          logger.error('Fan-out job failed — will retry', {
            messageId,
            postId: job.postId,
            error: String(err),
          });
          // Don't ACK — message remains in PEL for reclaim/retry
        }
      }
    } catch (err) {
      logger.error('Fan-out worker error', { error: String(err) });
      await sleep(1000);
    }
  }
}

async function reclaimStuck(redis: ReturnType<typeof getRedis>, stream: string): Promise<void> {
  try {
    // XAUTOCLAIM reclaims messages idle for CLAIM_IDLE_MS
    const result = await redis.xautoclaim(
      stream, CONSUMER_GROUP, CONSUMER_NAME,
      CLAIM_IDLE_MS, '0-0', 'COUNT', 50
    ) as [string, Array<[string, string[]]>];

    const [, messages] = result;
    if (messages.length > 0) {
      logger.warn(`Reclaimed ${messages.length} stuck fan-out messages`);
    }
  } catch (err) {
    // xautoclaim not available on older Redis — ignore
    logger.debug('xautoclaim not available', { error: String(err) });
  }
}

function sleep(ms: number): Promise<void> {
  return new Promise(resolve => setTimeout(resolve, ms));
}

// Graceful shutdown
process.on('SIGTERM', async () => {
  logger.info('Fan-out worker shutting down');
  process.exit(0);
});

run().catch(err => {
  logger.error('Fan-out worker crashed', { error: String(err) });
  process.exit(1);
});
