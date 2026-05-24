/**
 * k6 Load Test: Celebrity Spike Simulation
 *
 * Simulates a high-follower account posting (the "blast radius" scenario).
 * Measures:
 *   - API response time for post creation
 *   - Fan-out queue depth growth
 *   - WebSocket delivery latency
 *   - System behavior under sustained load
 *
 * Run: k6 run tests/load/celebrity-spike.js
 * Run (with output): k6 run --out json=results.json tests/load/celebrity-spike.js
 */
import http from 'k6/http';
import ws from 'k6/ws';
import { check, sleep } from 'k6';
import { Counter, Rate, Trend } from 'k6/metrics';

// ─── Custom Metrics ─────────────────────────────────────────────────────────────
const postCreationErrors = new Counter('post_creation_errors');
const feedLoadErrors     = new Counter('feed_load_errors');
const postCreationTime   = new Trend('post_creation_time_ms');
const feedLoadTime       = new Trend('feed_load_time_ms');
const wsConnectSuccess   = new Rate('ws_connect_success_rate');
const notifDeliveryTime  = new Trend('notification_delivery_ms');

// ─── Test Configuration ─────────────────────────────────────────────────────────
const BASE_URL = __ENV.BASE_URL || 'http://localhost:3000';
const WS_URL   = __ENV.WS_URL   || 'ws://localhost:3000';

// Simulated user IDs (pre-seeded in the database)
const NORMAL_USER_IDS    = Array.from({ length: 100 }, (_, i) => `user-normal-${i}`);
const CELEBRITY_USER_ID  = 'user-celebrity-1';  // 1M followers
const FOLLOWER_USER_IDS  = Array.from({ length: 1000 }, (_, i) => `user-follower-${i}`);

export const options = {
  scenarios: {
    // ── Scenario 1: Baseline normal load ──────────────────────────────────
    normal_posts: {
      executor: 'constant-arrival-rate',
      rate: 50,          // 50 posts/sec from small accounts
      timeUnit: '1s',
      duration: '2m',
      preAllocatedVUs: 20,
      maxVUs: 50,
      exec: 'normalPost',
    },

    // ── Scenario 2: Celebrity spike (the main event) ────────────────────
    celebrity_spike: {
      executor: 'per-vu-iterations',
      vus: 1,
      iterations: 5,     // Celebrity posts 5 times
      startTime: '30s',  // After baseline is established
      exec: 'celebrityPost',
    },

    // ── Scenario 3: Feed readers (concurrent users reading their feed) ─────
    feed_readers: {
      executor: 'constant-vus',
      vus: 200,
      duration: '2m',
      exec: 'readFeed',
    },

    // ── Scenario 4: WebSocket connections (notification listeners) ────────
    ws_listeners: {
      executor: 'constant-vus',
      vus: 50,
      duration: '2m',
      exec: 'wsListener',
    },
  },

  thresholds: {
    // Post creation should complete in < 200ms at p95
    'post_creation_time_ms': ['p(95)<200'],
    // Feed load should complete in < 500ms at p95
    'feed_load_time_ms': ['p(95)<500'],
    // Error rates
    'post_creation_errors': ['count<10'],
    'feed_load_errors': ['count<20'],
    // WS connections
    'ws_connect_success_rate': ['rate>0.95'],
    // HTTP error rate
    'http_req_failed': ['rate<0.05'],
  },
};

// ─── Scenario Functions ───────────────────────────────────────────────────────

/**
 * Normal user posting (small account, push fan-out)
 */
export function normalPost() {
  const userId = NORMAL_USER_IDS[Math.floor(Math.random() * NORMAL_USER_IDS.length)];
  const start = Date.now();

  const res = http.post(
    `${BASE_URL}/api/posts`,
    JSON.stringify({
      content: `Test post from normal user at ${new Date().toISOString()} #loadtest`,
      authorId: userId,
    }),
    {
      headers: {
        'Content-Type': 'application/json',
        'x-user-id': userId,
      },
    }
  );

  postCreationTime.add(Date.now() - start);

  const ok = check(res, {
    'normal post: status 201': (r) => r.status === 201,
    'normal post: has postId': (r) => {
      try { return !!JSON.parse(r.body).data?.id; } catch { return false; }
    },
    'normal post: strategy is push or hybrid': (r) => {
      try {
        const meta = JSON.parse(r.body).meta;
        return ['push', 'hybrid'].includes(meta?.fanoutStrategy);
      } catch { return false; }
    },
  });

  if (!ok) postCreationErrors.add(1);
  sleep(0.1);
}

/**
 * Celebrity post (1M followers, pull/hint strategy)
 * This is the "blast radius" scenario — should NOT cause O(1M) writes
 */
export function celebrityPost() {
  const start = Date.now();

  const res = http.post(
    `${BASE_URL}/api/posts`,
    JSON.stringify({
      content: `BREAKING: Celebrity post at ${new Date().toISOString()} — this triggers the hint path 🚀`,
      authorId: CELEBRITY_USER_ID,
    }),
    {
      headers: {
        'Content-Type': 'application/json',
        'x-user-id': CELEBRITY_USER_ID,
      },
    }
  );

  postCreationTime.add(Date.now() - start);

  check(res, {
    'celebrity post: status 201': (r) => r.status === 201,
    'celebrity post: strategy is pull': (r) => {
      try {
        return JSON.parse(r.body).meta?.fanoutStrategy === 'pull';
      } catch { return false; }
    },
    // KEY CHECK: API should respond quickly despite huge follower count
    // (fan-out is async; response time should NOT scale with follower count)
    'celebrity post: API response < 100ms': (r) => r.timings.duration < 100,
  });

  console.log(`Celebrity post created in ${Date.now() - start}ms`);
  sleep(10);  // Celebrity posts every ~10 seconds
}

/**
 * Feed reading (continuous — simulates users browsing their feeds)
 */
export function readFeed() {
  const userId = FOLLOWER_USER_IDS[Math.floor(Math.random() * FOLLOWER_USER_IDS.length)];
  const start = Date.now();

  const res = http.get(`${BASE_URL}/api/posts/feed/${userId}`, {
    headers: { 'x-user-id': userId },
  });

  feedLoadTime.add(Date.now() - start);

  const ok = check(res, {
    'feed: status 200': (r) => r.status === 200,
    'feed: has items array': (r) => {
      try { return Array.isArray(JSON.parse(r.body).data?.items); } catch { return false; }
    },
  });

  if (!ok) feedLoadErrors.add(1);
  sleep(Math.random() * 2 + 0.5);  // Read every 0.5–2.5s
}

/**
 * WebSocket listener — simulates connected clients waiting for notifications
 */
export function wsListener() {
  const userId = FOLLOWER_USER_IDS[Math.floor(Math.random() * FOLLOWER_USER_IDS.length)];
  let connected = false;
  let messagesReceived = 0;

  const res = ws.connect(
    `${WS_URL}?userId=${userId}`,
    { tags: { userId } },
    (socket) => {
      socket.on('open', () => {
        connected = true;
        wsConnectSuccess.add(1);
        // Send auth
        socket.send(JSON.stringify({ type: 'auth', userId, lastSeq: 0 }));
      });

      socket.on('message', (data) => {
        messagesReceived++;
        try {
          const event = JSON.parse(data);
          if (event.type === 'new_post' || event.type === 'new_hint') {
            notifDeliveryTime.add(Date.now() - new Date(event.timestamp).getTime());
          }
          // ACK the event
          socket.send(JSON.stringify({ type: 'ack', seq: event.seq }));
        } catch {}
      });

      socket.on('error', () => {
        wsConnectSuccess.add(0);
      });

      // Hold connection for 90 seconds
      socket.setTimeout(() => socket.close(), 90_000);
    }
  );

  if (!connected) wsConnectSuccess.add(0);

  check(res, {
    'ws: connected successfully': () => connected,
  });
}
