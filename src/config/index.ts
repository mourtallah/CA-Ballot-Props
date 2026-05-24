import dotenv from 'dotenv';
dotenv.config();

function requireEnv(key: string, fallback?: string): string {
  const val = process.env[key] ?? fallback;
  if (val === undefined) throw new Error(`Missing required env var: ${key}`);
  return val;
}
function num(key: string, fallback: number): number {
  const val = process.env[key];
  return val !== undefined ? parseInt(val, 10) : fallback;
}
function bool(key: string, fallback: boolean): boolean {
  const val = process.env[key];
  if (val === undefined) return fallback;
  return val === 'true' || val === '1';
}

export const config = {
  env: requireEnv('NODE_ENV', 'development'),
  port: num('PORT', 3000),
  wsPort: num('WS_PORT', 3001),
  wsServerId: requireEnv('WS_SERVER_ID', 'ws-server-1'),
  db: {
    host:     requireEnv('DB_HOST', 'localhost'),
    port:     num('DB_PORT', 5432),
    name:     requireEnv('DB_NAME', 'notifeed'),
    user:     requireEnv('DB_USER', 'notifeed_user'),
    password: requireEnv('DB_PASSWORD', 'notifeed_pass'),
    poolMin:  num('DB_POOL_MIN', 2),
    poolMax:  num('DB_POOL_MAX', 10),
  },
  redis: {
    host:     requireEnv('REDIS_HOST', 'localhost'),
    port:     num('REDIS_PORT', 6379),
    password: process.env.REDIS_PASSWORD ?? '',
    db:       num('REDIS_DB', 0),
  },
  fanout: {
    threshold:        num('FANOUT_THRESHOLD', 10_000),
    hybridThreshold:  num('FANOUT_HYBRID_THRESHOLD', 1_000),
    hintFraction:     parseFloat(process.env.CELEBRITY_HINT_FRACTION ?? '0.1'),
    rateLimitPerSec:  num('FANOUT_RATE_LIMIT_PER_SEC', 5_000),
    inboxTtlSeconds:  num('INBOX_TTL_SECONDS', 86_400),
  },
  reliability: {
    replayBufferSize: num('REPLAY_BUFFER_SIZE', 500),
    replayTtlSeconds: num('REPLAY_TTL_SECONDS', 300),
  },
  metrics: {
    port:    num('METRICS_PORT', 9090),
    enabled: bool('ENABLE_METRICS', true),
  },
  features: {
    celebrityHints: bool('ENABLE_CELEBRITY_HINTS', true),
    replayBuffer:   bool('ENABLE_REPLAY_BUFFER', true),
  },
  jwt: {
    secret: requireEnv('JWT_SECRET', 'dev-secret-change-in-production'),
    expiry: requireEnv('JWT_EXPIRY', '7d'),
  },
} as const;
