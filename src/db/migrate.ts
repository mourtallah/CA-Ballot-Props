import fs from 'fs';
import path from 'path';
import { pool } from './client';
import { logger } from '../lib/logger';

async function migrate(): Promise<void> {
  const dir = path.join(__dirname, 'migrations');
  const files = fs.readdirSync(dir).filter(f => f.endsWith('.sql')).sort();
  const client = await pool.connect();
  try {
    await client.query(`CREATE TABLE IF NOT EXISTS _migrations (
      filename TEXT PRIMARY KEY, applied_at TIMESTAMPTZ DEFAULT NOW()
    )`);
    for (const file of files) {
      const { rows } = await client.query('SELECT 1 FROM _migrations WHERE filename=$1', [file]);
      if (rows.length > 0) { logger.info(`Skip: ${file}`); continue; }
      const sql = fs.readFileSync(path.join(dir, file), 'utf-8');
      logger.info(`Applying: ${file}`);
      await client.query('BEGIN');
      await client.query(sql);
      await client.query('INSERT INTO _migrations(filename) VALUES($1)', [file]);
      await client.query('COMMIT');
      logger.info(`Applied: ${file}`);
    }
    logger.info('Migrations complete');
  } catch (err) {
    await client.query('ROLLBACK');
    throw err;
  } finally {
    client.release();
    await pool.end();
  }
}

migrate().catch(err => { console.error(err); process.exit(1); });
