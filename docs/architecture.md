# Architecture: Hybrid Notification & Feed System

## Overview

This system demonstrates the core engineering challenges of real-time notification
delivery at scale, as seen in systems like Twitter/X, Instagram, and Discord.

The central question: **when a user posts, how do you notify potentially millions of followers?**

---

## The Fan-Out Problem

```
User A posts → 1,000,000 followers need to know

Naïve push:  1 write → 1,000,000 inbox writes (DISASTER at scale)
Pure pull:   0 writes → 1,000,000 DB reads on open (thundering herd)
Hybrid:      1 write → smart routing based on follower count
```

---

## Strategy Matrix

| Follower Count | Strategy | What Happens |
|---------------|----------|--------------|
| < 1,000       | **PUSH** | Write post to every follower's inbox immediately |
| 1,000–10,000  | **HYBRID** | Write to *active* followers only; passive pull on open |
| > 10,000      | **PULL** | Push lightweight hints to top active followers; everyone else pulls |

---

## System Architecture

```
┌─────────────────────────────────────────────────────────────────┐
│                        CLIENT LAYER                              │
│  Browser / Mobile App                                            │
│  ┌──────────────────┐         ┌──────────────────────────────┐  │
│  │   WebSocket/SSE  │         │   REST API (feed/timeline)   │  │
│  │  (real-time push)│         │   (pull path, pagination)    │  │
│  └────────┤─────────┘         └──────────────┬───────────────┘  │
└───────────┬───────────────────────────────┬───────────────────┘
            │                                   │
┌───────────┼───────────────────────┼──────────────────┐
│           │         API GATEWAY LAYER         │                  │
│  ┌────────▼──────────────────────────▼──────────────┐   │
│  │                WebSocket Server(s)                       │   │
│  │    Socket.io  ·  Connection Registry (Redis)             │   │
│  │    Replay Buffer  ·  Sequence Numbers  ·  SSE fallback   │   │
│  └─────────────────────────┬────────────────────────┘   │
└───────────────────────────┼──────────────────────────────────────┘
                              │
┌───────────────────────────┼──────────────────────────────────────┐
│                  PROCESSING LAYER                                │
│                             │                                    │
│  POST CREATION              │              FEED ASSEMBLY         │
│  ┌──────────────────┐       │       ┌─────────────────────┐   │
│  │  API Handler     │       │       │  Feed Service         │   │
│  │  1. Write post   │       │       │  1. Read Redis inbox  │   │
│  │  2. Check count  │       │       │  2. Pull from DB for  │   │
│  │  3. Enqueue job  │       │       │     celebrity follows │   │
│  └───────┤─────────┘       │       │  3. Merge + rank      │   │
│           │                 │       └─────────────────────┘   │
│  ┌────────▼──────────────────────────────────────────────────┐  │
│  │              Redis Streams (Message Broker)                │  │
│  │   fanout:queue ──────────► Fan-out Worker(s)                  │  │
│  │   notification:queue ► Notification Worker(s)             │  │
│  └───────────────────────────────────────────────────────────┘  │
└─────────────────────────────────────────────────────────────────┘
                              │
┌───────────────────────────┼──────────────────────────────────────┐
│                  STORAGE LAYER                                   │
│                             │                                    │
│  ┌───────────────┐    ┌─────▼──────────┐    ┌────────────────┐ │
│  │  PostgreSQL   │    │     Redis       │    │  Redis Streams │ │
│  │               │    │                 │    │                │ │
│  │  users        │    │  user:inbox:*   │    │  fanout:queue  │ │
│  │  posts        │    │  (push inboxes) │    │  notif:queue   │ │
│  │  follows      │    │  replay:*       │    │  replay:*      │ │
│  │  notifications│    │  (replay bufs)  │    │  (per-user)    │ │
│  │  fanout_events│    │  active:users   │    │                │ │
│  └───────────────┘    │  ws:server:*    │    └────────────────┘ │
│                       └─────────────────┘                       │
└─────────────────────────────────────────────────────────────────┘
```

---

## Sequence Diagrams

### Small Account Post (Push Fan-out)

```
Client      API      DB       Redis(Stream)  Fan-out Worker  Followers' Clients
  │          │        │              │               │                │
  │─ POST ──►│        │              │               │                │
  │          │─ INSERT post ────────►│               │                │
  │          │        │◄─ post ──────│               │                │
  │          │─ XADD fanout:queue ──►│               │                │
  │◄─ 201 ───│        │              │               │                │
  │          │        │              │◄─ XREADGROUP ─┤                │
  │          │        │              │── job ────────►│                │
  │          │        │              │               │─ ZADD inbox:* ─► (Redis)
  │          │        │              │               │─ XADD notif:queue
  │          │        │              │◄──────────────┤ (per follower)  │
  │          │        │              │               │                 │
  │          │        │              │  Notification Worker           │
  │          │        │              │◄─ XREADGROUP ────────────────┤
  │          │        │              │               │─ socket.emit ──►│
  │          │        │              │               │  new_post       │
```

### Celebrity Post (Pull / Hint Fan-out)

```
Client      API      DB       Redis       Fan-out Worker   Active Followers
  │          │        │         │               │                │
  │─ POST ──►│        │         │               │                │
  │          │─ INSERT ────────►│               │                │
  │          │─ XADD fanout:q ─►│               │                │
  │◄─ 201 ───│        │         │◄─ XREADGROUP ─┤                │
  │  (fast! no wait)            │── job ────────►│                │
  │          │        │         │               │                │
  │          │        │ ← DB query: top active followers (10%)   │
  │          │        │────────────────────────────────────────────►│
  │          │        │         │               │                │
  │          │        │         │◄──────────────┤ ZADD hints:*   │
  │          │        │         │               │ XADD notif:q   │
  │          │        │         │  Notification Worker fires "hint" to WS  │
  │          │        │────────────────────────────────────────────►│
  │          │        │               │  socket.emit new_hint (no content)
  │          │        │               │                │
  │          │        │         │  Follower fetches feed on demand:
  │          │        │◄─ GET /feed/userId ──────────────────────┤
  │          │        │── posts ───────────────────────────────────────►│
```

### Reconnect & Replay

```
Client               WebSocket Server              Redis
  │                        │                         │
  │── connect(lastSeq=47) ─►│                         │
  │                        │─ XRANGE replay:{userId} ─►│
  │                        │◄── events [48..55] ───────│
  │◄── replay_start ───────│                         │
  │◄── event(seq=48) ──────│                         │
  │◄── event(seq=49) ──────│                         │
  │   ... (missed events)  │                         │
  │◄── event(seq=55) ──────│                         │
  │◄── replay_end ─────────│                         │
  │── ack(seq=55) ─────────►│                         │
  │                        │─ SET user:seq:{id} 55 ──►│
```

---

## Key Design Decisions

### 1. Why Redis Streams (not plain Pub/Sub)?
- **Redis Pub/Sub**: fire-and-forget. Missed messages if consumer is down.
- **Redis Streams**: durable log, consumer groups, ACK/replay semantics.
- For fan-out jobs and notification delivery, at-least-once delivery is critical.

### 2. Why not fan-out everything on read?
- Pure pull works if you have O(1) followers. At 1M followers, assembling a
  timeline requires reading from 1M accounts' post tables — impossible without
  heavy caching.
- Hybrid: pre-materialise for small accounts (fast read, acceptable write cost);
  query on demand for celebrities (bounded read cost even at 1M follows).

### 3. Replay Buffer Design
```
User reconnects after 2 minutes offline.
Redis Stream: [evt@seq=50, evt@seq=51, ..., evt@seq=67]
Client sends lastSeq=49 → replays 50..67 (18 missed events)
Client deduplicates by seq number (idempotent delivery)
```

### 4. Connection Routing (Multi-Server)
```
Redis key: user:connections:{userId} = SET of server IDs
Redis key: ws:server:{serverId}:users = SET of userIds

Fan-out worker → posts to notification:queue
Notification worker on each server → checks if recipient is local
  YES → socket.emit() directly
  NO  → skip (another server's worker will handle it)
```

---

## Trade-off Analysis

| Dimension | Push | Pull | Hybrid |
|-----------|------|------|--------|
| Write cost | O(followers) | O(1) | O(active_followers × fraction) |
| Read latency | O(1) from inbox | O(follows × posts) | Mixed |
| Freshness | Immediate | Depends on poll interval | Near-real-time for active users |
| Storage | High (inbox per user) | Low (single canonical) | Medium |
| Complexity | Low | Medium | High |

**Rule of thumb**: Start with push, add pull path when follower counts exceed 10K.

---

## Observability

### Key Metrics to Watch
1. `notifeed_fanout_duration_ms{strategy="push"}` — should be < 100ms for < 1K followers
2. `notifeed_fanout_target_size{strategy="pull"}` — blast radius for celebrity posts
3. `notifeed_ws_connections_active` — connection pool health
4. `notifeed_feed_request_duration_ms` — feed assembly latency
5. `notifeed_queue_depth{queue="fanout:queue"}` — backlog (alert if > 10K)

### Alert Conditions
- Fan-out queue depth > 50K for > 5 minutes → fan-out workers overwhelmed
- Feed p95 > 1s → pull queries hitting unindexed paths
- WS connection drop > 20% in 1 minute → network partition or OOM

---

## Scaling Playbook

| Load Level | Action |
|-----------|--------|
| 10K concurrent | Single instance, Redis Streams |
| 100K concurrent | Multiple WS servers + Redis pub/sub for routing |
| 1M concurrent | Kafka instead of Redis Streams, geographic sharding |
| 10M+ | Separate notification microservice, CDN for hint delivery |
