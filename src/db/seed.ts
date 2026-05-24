import { pool, withTransaction } from './client';
import { logger } from '../lib/logger';

const NORMAL_USER_COUNT = 500;
const MID_TIER_COUNT = 10;

async function seed(): Promise<void> {
  logger.info('Seeding database...');
  await withTransaction(async (client) => {
    // Celebrity user (1M followers, pull strategy)
    await client.query(`
      INSERT INTO users (id, username, display_name, bio, follower_count, fanout_strategy)
      VALUES ('a0000000-0000-0000-0000-000000000001','celebrity1','Big Celebrity Account',
              'Official account. 1M followers. Posts use pull fan-out.',1000000,'pull')
      ON CONFLICT (username) DO UPDATE SET follower_count=1000000, fanout_strategy='pull'
    `);
    // Mid-tier users (5K followers, hybrid strategy)
    for (let i = 1; i <= MID_TIER_COUNT; i++) {
      await client.query(
        `INSERT INTO users (username,display_name,follower_count,fanout_strategy)
         VALUES ($1,$2,5000,'hybrid')
         ON CONFLICT (username) DO UPDATE SET follower_count=5000,fanout_strategy='hybrid'`,
        [`midtier_${i}`, `Mid-Tier User ${i}`]
      );
    }
    // Normal users (< 1K followers, push strategy)
    for (let i = 1; i <= NORMAL_USER_COUNT; i++) {
      const followers = Math.floor(Math.random() * 900) + 10;
      await client.query(
        `INSERT INTO users (username,display_name,follower_count,fanout_strategy)
         VALUES ($1,$2,$3,'push') ON CONFLICT (username) DO NOTHING`,
        [`user_${i}`, `User ${i}`, followers]
      );
    }
    logger.info(`Seeded: 1 celebrity, ${MID_TIER_COUNT} mid-tier, ${NORMAL_USER_COUNT} normal users`);
  });
  await pool.end();
}

seed().catch(err => { logger.error('Seed failed', { error: String(err) }); process.exit(1); });
