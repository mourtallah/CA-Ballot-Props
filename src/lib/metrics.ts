import { Registry, Counter, Histogram, Gauge, collectDefaultMetrics } from 'prom-client';
import { config } from '../config';

export const registry = new Registry();
if (config.metrics.enabled) collectDefaultMetrics({ register: registry });

export const fanoutTotal = new Counter({
  name: 'notifeed_fanout_operations_total',
  help: 'Total fan-out operations dispatched',
  labelNames: ['strategy'],
  registers: [registry],
});

export const fanoutDuration = new Histogram({
  name: 'notifeed_fanout_duration_ms',
  help: 'Fan-out duration in milliseconds',
  labelNames: ['strategy'],
  buckets: [1, 5, 10, 25, 50, 100, 250, 500, 1000, 2500, 5000],
  registers: [registry],
});

export const fanoutTargetSize = new Histogram({
  name: 'notifeed_fanout_target_size',
  help: 'Number of followers targeted per fan-out',
  labelNames: ['strategy'],
  buckets: [1, 10, 100, 500, 1_000, 5_000, 10_000, 50_000, 100_000, 500_000, 1_000_000],
  registers: [registry],
});

export const wsConnectionsActive = new Gauge({
  name: 'notifeed_ws_connections_active',
  help: 'Number of active WebSocket connections on this server',
  registers: [registry],
});

export const wsMessagesDelivered = new Counter({
  name: 'notifeed_ws_messages_delivered_total',
  help: 'Total messages delivered via WebSocket',
  labelNames: ['type'],
  registers: [registry],
});

export const wsReplayEvents = new Counter({
  name: 'notifeed_ws_replay_events_total',
  help: 'Total events replayed on reconnect',
  registers: [registry],
});

export const feedRequestDuration = new Histogram({
  name: 'notifeed_feed_request_duration_ms',
  help: 'Feed assembly latency in milliseconds',
  labelNames: ['path'],
  buckets: [5, 10, 25, 50, 100, 250, 500, 1000],
  registers: [registry],
});

export const queueDepth = new Gauge({
  name: 'notifeed_queue_depth',
  help: 'Current depth of Redis Stream queues',
  labelNames: ['queue'],
  registers: [registry],
});

export const postsCreatedTotal = new Counter({
  name: 'notifeed_posts_created_total',
  help: 'Total posts created',
  labelNames: ['fanout_strategy'],
  registers: [registry],
});
