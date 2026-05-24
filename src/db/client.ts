import { Pool, PoolClient } from 'pg';
import { config } from '../config';
import { logger } from '../lib/logger';

export const pool = new Pool({
  host: config.db.host, port: config.db.port,
  database: config.db.name, user: config.db.user,
  password: config.db.password,
  min: config.db.poolMin, max: config.db.poolMax,
  idleTimeoutMillis: 30_000,
  connectionTimeoutMillis: 5_000,
  statement_timeout: 10_000,
});

pool.on('error', (err) => logger.error('PostgreSQL pool error', { error: err.message }));

export async function query<T extends object = Record<string, unknown>>(
  text: string, params?: unknown[]
): Promise<T[]> {
  const start = Date.now();
  const result = await pool.query(text, params);
  const duration = Date.now() - start;
  if (config.env === 'development' && duration > 200)
    logger.warn('Slow query', { query: text.slice(0, 80), duration });
  return result.rows as T[];
}

export async function withTransaction<T>(
  fn: (client: PoolClient) => Promise<T>
): Promise<T> {
  const client = await pool.connect();
  try {
    await client.query('BEGIN');
    const result = await fn(client);
    await client.query('COMMIT');
    return result;
  } catch (err) {
    await client.query('ROLLBACK');
    throw err;
  } finally {
    client.release();
  }
}
