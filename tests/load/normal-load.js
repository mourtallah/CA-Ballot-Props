/**
 * k6 Load Test: Normal baseline load
 * Run: k6 run tests/load/normal-load.js
 */
import http from 'k6/http';
import { check, sleep } from 'k6';
import { Trend, Counter } from 'k6/metrics';

const BASE_URL = __ENV.BASE_URL || 'http://localhost:3000';
const postDuration = new Trend('post_duration_ms');
const feedDuration = new Trend('feed_duration_ms');
const errors       = new Counter('errors');

export const options = {
  stages: [
    { duration: '30s', target: 50  },   // Ramp up
    { duration: '2m',  target: 200 },   // Sustained load
    { duration: '30s', target: 500 },   // Spike
    { duration: '1m',  target: 200 },   // Recovery
    { duration: '30s', target: 0   },   // Ramp down
  ],
  thresholds: {
    'http_req_duration': ['p(95)<300'],
    'http_req_failed': ['rate<0.01'],
    'post_duration_ms': ['p(99)<500'],
  },
};

const USER_COUNT = 500;
function randomUserId() {
  return `user-${Math.floor(Math.random() * USER_COUNT)}`;
}

export default function () {
  const userId = randomUserId();

  // 70% reads, 30% writes (realistic ratio)
  if (Math.random() < 0.7) {
    // Read feed
    const start = Date.now();
    const res = http.get(`${BASE_URL}/api/posts/feed/${userId}`);
    feedDuration.add(Date.now() - start);
    if (!check(res, { 'feed 200': (r) => r.status === 200 })) errors.add(1);
  } else {
    // Create post
    const start = Date.now();
    const res = http.post(
      `${BASE_URL}/api/posts`,
      JSON.stringify({ content: `Post ${Date.now()}`, authorId: userId }),
      { headers: { 'Content-Type': 'application/json', 'x-user-id': userId } }
    );
    postDuration.add(Date.now() - start);
    if (!check(res, { 'post 201': (r) => r.status === 201 })) errors.add(1);
  }

  sleep(0.5 + Math.random());
}
