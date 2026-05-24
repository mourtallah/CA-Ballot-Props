/**
 * Main application entry point.
 * Starts the HTTP/WebSocket server, connects to DB/Redis, registers routes.
 */
import http from 'http';
import express from 'express';
import cors from 'cors';
import helmet from 'helmet';
import { config } from './config';
import { logger } from './lib/logger';
import { registry } from './lib/metrics';
import { pool } from './db/client';
import { getRedis } from './db/redis';
import { initWebSocketServer } from './services/realtime/websocket.service';
import { postsRouter } from './api/routes/posts.routes';
import { usersRouter } from './api/routes/users.routes';

async function bootstrap(): Promise<void> {
  // ── Express app ─────────────────────────────────────────────────────────
  const app = express();

  app.use(helmet());
  app.use(cors());
  app.use(express.json({ limit: '10kb' }));

  // ── Routes ───────────────────────────────────────────────────────────────
  app.use('/api/users', usersRouter);
  app.use('/api/posts', postsRouter);

  // ── Health check ─────────────────────────────────────────────────────────
  app.get('/health', async (_req, res) => {
    try {
      await pool.query('SELECT 1');
      const redis = getRedis();
      await redis.ping();
      res.json({ status: 'ok', serverId: config.wsServerId });
    } catch (err) {
      res.status(503).json({ status: 'degraded', error: String(err) });
    }
  });

  // ── Prometheus metrics ────────────────────────────────────────────────────
  if (config.metrics.enabled) {
    app.get('/metrics', async (_req, res) => {
      res.set('Content-Type', registry.contentType);
      res.end(await registry.metrics());
    });
  }

  // ── HTTP + WebSocket server ───────────────────────────────────────────────
  const server = http.createServer(app);
  initWebSocketServer(server);

  // ── Start ─────────────────────────────────────────────────────────────────
  server.listen(config.port, () => {
    logger.info(`🚀 NotiFeeder API ready`, {
      port: config.port,
      env: config.env,
      serverId: config.wsServerId,
      fanoutThreshold: config.fanout.threshold,
    });
  });

  // ── Graceful shutdown ─────────────────────────────────────────────────────
  async function shutdown(signal: string): Promise<void> {
    logger.info(`Received ${signal} — shutting down gracefully`);
    server.close(async () => {
      await pool.end();
      const redis = getRedis();
      await redis.quit();
      logger.info('All connections closed');
      process.exit(0);
    });
    // Force exit after 10s
    setTimeout(() => process.exit(1), 10_000);
  }

  process.on('SIGTERM', () => shutdown('SIGTERM'));
  process.on('SIGINT',  () => shutdown('SIGINT'));
}

bootstrap().catch(err => {
  logger.error('Fatal startup error', { error: String(err) });
  process.exit(1);
});
