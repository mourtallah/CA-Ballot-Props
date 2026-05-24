/**
 * WebSocket Service (Socket.io)
 *
 * Handles real-time delivery of events to connected clients.
 *
 * Key responsibilities:
 *   - Track which users are connected to THIS server instance
 *   - Register connection location in Redis (for cross-server routing)
 *   - Replay missed events on reconnect (using per-user replay buffer)
 *   - Handle graceful disconnect / cleanup
 *
 * Cross-server routing: When a fan-out worker needs to deliver to user U
 * but U is connected to ws-server-2, it publishes to Redis Streams and
 * each WS server checks if the recipient is local.
 */
import { Server as HttpServer } from 'http';
import { Server as SocketServer, Socket } from 'socket.io';
import { getRedis, Keys } from '../../db/redis';
import { config } from '../../config';
import { logger } from '../../lib/logger';
import {
  wsConnectionsActive,
  wsMessagesDelivered,
  wsReplayEvents,
} from '../../lib/metrics';
import * as userModel from '../../models/user.model';
import type { RealtimeEvent, ClientConnectOptions } from '../../types';

// Map: userId → Set of socket IDs (a user can have multiple tabs open)
const localConnections = new Map<string, Set<string>>();

let io: SocketServer | null = null;

export function initWebSocketServer(httpServer: HttpServer): SocketServer {
  io = new SocketServer(httpServer, {
    cors: { origin: '*', methods: ['GET', 'POST'] },
    pingTimeout: 60_000,
    pingInterval: 25_000,
    transports: ['websocket', 'polling'],
  });

  io.on('connection', handleConnection);
  logger.info('WebSocket server initialized', { serverId: config.wsServerId });
  return io;
}

// ─── Connection Lifecycle ─────────────────────────────────────────────────

async function handleConnection(socket: Socket): Promise<void> {
  const { userId, lastSeq } = socket.handshake.auth as ClientConnectOptions;

  if (!userId) {
    logger.warn('WS connection rejected: no userId');
    socket.disconnect(true);
    return;
  }

  logger.info('Client connected', { userId, socketId: socket.id });
  wsConnectionsActive.inc();

  // Track locally
  if (!localConnections.has(userId)) {
    localConnections.set(userId, new Set());
  }
  localConnections.get(userId)!.add(socket.id);

  // Register in Redis: this server hosts this user
  const redis = getRedis();
  await redis.pipeline()
    .sadd(Keys.userConnections(userId), config.wsServerId)
    .sadd(Keys.wsServerUsers(config.wsServerId), userId)
    .expire(Keys.userConnections(userId), 3600)
    .exec();

  // Update user activity
  await userModel.updateLastActive(userId);

  // Replay missed events if client provides lastSeq
  if (lastSeq !== undefined) {
    await replayMissedEvents(socket, userId, lastSeq);
  }

  socket.join(`user:${userId}`);
  socket.emit('connected', { serverId: config.wsServerId });

  // ── Event Handlers ────────────────────────────────────────────────────
  socket.on('ack', async (data: { seq: number }) => {
    // Client acknowledges receiving up to sequence `seq`
    await redis.set(Keys.userSeq(userId), data.seq);
  });

  socket.on('ping', () => {
    socket.emit('pong', { ts: Date.now() });
  });

  socket.on('disconnect', async (reason) => {
    logger.info('Client disconnected', { userId, reason });
    wsConnectionsActive.dec();

    const sockets = localConnections.get(userId);
    if (sockets) {
      sockets.delete(socket.id);
      if (sockets.size === 0) {
        localConnections.delete(userId);
        // Remove this server from user's connection set in Redis
        await redis.pipeline()
          .srem(Keys.userConnections(userId), config.wsServerId)
          .srem(Keys.wsServerUsers(config.wsServerId), userId)
          .exec();
      }
    }
  });
}

// ─── Event Delivery ───────────────────────────────────────────────────────

/**
 * Deliver an event to a user if they're connected to this server.
 * Also appends to their replay buffer for reliability.
 */
export async function deliverToUser(event: RealtimeEvent): Promise<boolean> {
  if (!io) return false;

  const isLocal = localConnections.has(event.userId);
  if (!isLocal) return false;

  // Append to replay buffer BEFORE delivery
  if (config.features.replayBuffer) {
    await appendToReplayBuffer(event);
  }

  io.to(`user:${event.userId}`).emit(event.type, event);
  wsMessagesDelivered.inc({ type: event.type });
  return true;
}

/**
 * Broadcast to all connected clients on this server (e.g., system alerts).
 */
export function broadcastAll(eventType: string, payload: unknown): void {
  if (!io) return;
  io.emit(eventType, payload);
}

// ─── Replay Buffer ────────────────────────────────────────────────────────

/**
 * Append event to user's per-user Redis Stream replay buffer.
 * Trimmed to REPLAY_BUFFER_SIZE messages; expires after REPLAY_TTL_SECONDS.
 */
async function appendToReplayBuffer(event: RealtimeEvent): Promise<void> {
  const redis = getRedis();
  const key = Keys.replayBuffer(event.userId);
  await redis.pipeline()
    .xadd(key, '*', 'event', JSON.stringify(event))
    .xtrim(key, 'MAXLEN', '~', config.reliability.replayBufferSize)
    .expire(key, config.reliability.replayTtlSeconds)
    .exec();
}

/**
 * On reconnect: replay all events the client missed since `lastSeq`.
 * We find the corresponding Redis Stream ID via sequence number mapping.
 */
async function replayMissedEvents(
  socket: Socket,
  userId: string,
  lastSeq: number
): Promise<void> {
  const redis = getRedis();
  const key = Keys.replayBuffer(userId);

  try {
    // Read all events in replay buffer
    const entries = await redis.xrange(key, '-', '+');
    if (entries.length === 0) return;

    socket.emit('replay_start', { count: entries.length });

    let replayed = 0;
    for (const [, fields] of entries) {
      const eventJson = fields[1]; // fields = ['event', jsonString]
      if (!eventJson) continue;

      const event = JSON.parse(eventJson) as RealtimeEvent;
      if (event.seq > lastSeq) {
        socket.emit(event.type, event);
        replayed++;
      }
    }

    socket.emit('replay_end', { replayed });
    wsReplayEvents.inc(replayed);

    logger.info('Replay complete', { userId, lastSeq, replayed });
  } catch (err) {
    logger.error('Replay error', { userId, error: String(err) });
  }
}

// ─── Accessors ────────────────────────────────────────────────────────────

export function getLocalUserCount(): number {
  return localConnections.size;
}

export function isUserLocal(userId: string): boolean {
  return localConnections.has(userId);
}
