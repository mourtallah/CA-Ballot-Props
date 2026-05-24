# Hybrid Notification & Feed System

> **A production-inspired, educational implementation of scalable real-time notifications**  
> Exploring push vs. pull fan-out, the celebrity problem, WebSocket reliability, and hybrid strategies — the same engineering decisions behind Twitter/X, Instagram, and Discord.

[![TypeScript](https://img.shields.io/badge/TypeScript-5.3-blue.svg)](https://www.typescriptlang.org/)
[![Node.js](https://img.shields.io/badge/Node.js-20-green.svg)](https://nodejs.org/)
[![Redis](https://img.shields.io/badge/Redis-7-red.svg)](https://redis.io/)
[![PostgreSQL](https://img.shields.io/badge/PostgreSQL-16-blue.svg)](https://www.postgresql.org/)
[![Docker](https://img.shields.io/badge/Docker-Compose-2496ED.svg)](https://docs.docker.com/compose/)

---

## The Problem This Solves

When a user with **1,000,000 followers** posts a tweet, how does every follower find out?

```
Naïve push:  1 post → 1,000,000 inbox writes → broker meltdown 💀
Pure pull:   0 writes → 1,000,000 DB reads on next open → thundering herd 💀
This system: 1 post → ~50,000 smart hint deliveries + on-demand pull → ✅
```

This repo implements the **hybrid fan-out pattern** used in real social systems:
- **Small accounts** (< 1K followers): fan-out on **write** (push to all)
- **Mid-tier** (1K–10K): push to **active followers only**
- **Celebrity** (10K+): push lightweight **hints** to top active followers; everyone else assembles feed on read

---

## Architecture at a Glance

```
┌────────────────────────────────────────────────────────────────────┐
│                          CLIENT LAYER                               │
│         WebSocket/SSE (real-time)    REST API (feed/pull)           │
└──────────────────────────┬──────────────────────┬──────────────────┘
                           │                      │
┌──────────────────────────▼──────────────────────▼──────────────────┐
│                    WS SERVER (Socket.io)                             │
│   Connection Registry (Redis)  ·  Replay Buffer  ·  Seq Numbers     │
└──────────────────────────────────┬─────────────────────────────────┘
                                   │
┌──────────────────────────────────▼─────────────────────────────────┐
│                    REDIS STREAMS (Message Broker)                    │
│    fanout:queue ──────► Fan-out Workers (2 replicas)                │
│    notification:queue ► Notification Workers (2 replicas)           │
└─────────────────────────────────────────────────────────────────────┘
                          │               │
          ┌───────────────┘               └──────────────────┐
          ▼                                                   ▼
┌─────────────────────┐                        ┌─────────────────────┐
│      PostgreSQL      │                        │        Redis         │
│  users, posts,       │                        │  user:inbox:*       │
│  follows,            │                        │  replay:*           │
│  notifications,      │                        │  celebrity:hints:*  │
│  fanout_events       │                        │  active:users       │
└─────────────────────┘                        └─────────────────────┘
```

---

## Quick Start

### Prerequisites
- [Docker](https://docs.docker.com/get-docker/) + [Docker Compose](https://docs.docker.com/compose/install/)
- Node.js 20+ (for running tests / load tests locally)
- [k6](https://k6.io/docs/get-started/installation/) (for load testing, optional)

### 1. Start the full stack

```bash
git clone https://github.com/YOUR_USERNAME/hybrid-notification-feed.git
cd hybrid-notification-feed

cp .env.example .env

docker compose up --build
```

This starts:
| Service | Port | Purpose |
|---------|------|--------|
| API + WebSocket | `3000` | REST API & real-time WS |
| PostgreSQL | `5432` | Primary data store |
| Redis | `6379` | Cache, streams, pub/sub |
| Prometheus | `9090` | Metrics scraping |
| Grafana | `3030` | Dashboards (admin/admin) |

### 2. Run migrations & seed data

```bash
docker compose exec api npm run migrate
docker compose exec api npm run seed
```

### 3. Try it out

```bash
# Create a user
curl -X POST http://localhost:3000/api/users \
  -H "Content-Type: application/json" \
  -d '{"username":"alice","displayName":"Alice"}'

# Create a post (fan-out strategy auto-selected by follower count)
curl -X POST http://localhost:3000/api/posts \
  -H "Content-Type: application/json" \
  -H "x-user-id: <alice-id>" \
  -d '{"content":"Hello, world!"}'

# Get a feed
curl http://localhost:3000/api/posts/feed/<user-id>
```

### 4. Run tests

```bash
npm install
npm test
npm run test:unit
```

### 5. Run load tests (celebrity spike)

```bash
k6 run tests/load/celebrity-spike.js \
  --env BASE_URL=http://localhost:3000 \
  --env WS_URL=ws://localhost:3000
```

---

## Project Structure

```
hybrid-notification-feed-system/
├── src/
│   ├── index.ts                          # App entry point
│   ├── config/index.ts                   # All env vars, typed & validated
│   ├── types/index.ts                    # Shared TypeScript types
│   ├── db/
│   │   ├── client.ts                     # PostgreSQL pool + query helpers
│   │   ├── redis.ts                      # ioredis singleton + key naming
│   │   ├── migrations/001_init.sql       # Schema (users, posts, follows, ...)
│   │   └── seed.ts                       # Test data seeder
│   ├── lib/
│   │   ├── logger.ts                     # Winston structured logger
│   │   └── metrics.ts                    # Prometheus metrics registry
│   ├── models/
│   │   ├── user.model.ts                 # User CRUD + follow operations
│   │   └── post.model.ts                 # Post CRUD + batch fetch
│   ├── services/
│   │   ├── fanout/fanout.service.ts      # ⭐ Core hybrid fan-out logic
│   │   ├── feed/feed.service.ts          # Feed assembly (merge + rank)
│   │   └── realtime/websocket.service.ts # Socket.io + replay buffer
│   ├── api/routes/
│   │   ├── posts.routes.ts               # POST /posts, GET /posts/feed/:id
│   │   └── users.routes.ts               # Users, follows, notifications
│   └── workers/
│       ├── fanout-worker.ts              # Fan-out job consumer
│       └── notification-worker.ts        # Notification delivery consumer
├── tests/
│   ├── unit/
│   └── load/
│       ├── celebrity-spike.js            # k6: 1M-follower post simulation
│       └── normal-load.js
├── monitoring/                           # Prometheus + Grafana config
├── docs/
│   ├── architecture.md                   # Full architecture + sequence diagrams
│   └── trade-offs.md                     # Push vs Pull vs Hybrid deep-dive
├── k8s/deployment.yaml                   # K8s Deployment + HPA manifests
├── docker-compose.yml
├── Dockerfile
└── .env.example
```

---

## Core Concept: The Fan-out Decision Tree

```typescript
function resolveStrategy(followerCount: number): FanoutStrategy {
  if (followerCount >= 10_000) return 'pull';    // Celebrity: hints only
  if (followerCount >= 1_000)  return 'hybrid';  // Mid-tier: active followers
  return 'push';                                  // Small: fan-out to all
}
```

| Strategy | Writes per post | Delivery latency | Storage cost |
|----------|----------------|------------------|-------------|
| `push`   | O(followers)   | Immediate        | High        |
| `hybrid` | O(active_followers) | Near-instant | Medium    |
| `pull`   | O(active × hint_fraction) | On-open | Low      |

---

## Key Learning Outcomes

1. **Blast radius awareness** — One write should not cost O(N) at scale
2. **Push is fast; pull is cheap** — hybrid wins for mixed audiences
3. **Stateful real-time + reliability** — connections, routing, replays, sequence numbers
4. **Message brokers** (Redis Streams) enable at-least-once delivery
5. **Observability matters** — fan-out duration, queue depth, WS connection count
6. **Graceful degradation** — rate-limiting fan-out, falling back to pull-only

---

## Further Reading

- [`docs/architecture.md`](docs/architecture.md) — Full architecture with sequence diagrams
- [`docs/trade-offs.md`](docs/trade-offs.md) — Push vs Pull vs Hybrid deep-dive

---

## License

MIT
