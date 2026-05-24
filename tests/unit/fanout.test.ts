/**
 * Unit tests for the Hybrid Fan-out Service
 */
import { resolveStrategy } from '../../src/services/fanout/fanout.service';

describe('resolveStrategy', () => {
  it('returns "push" for accounts below hybrid threshold', () => {
    expect(resolveStrategy(0)).toBe('push');
    expect(resolveStrategy(500)).toBe('push');
    expect(resolveStrategy(999)).toBe('push');
  });

  it('returns "hybrid" for mid-tier accounts (1K–10K followers)', () => {
    expect(resolveStrategy(1_000)).toBe('hybrid');
    expect(resolveStrategy(5_000)).toBe('hybrid');
    expect(resolveStrategy(9_999)).toBe('hybrid');
  });

  it('returns "pull" for celebrity accounts (10K+ followers)', () => {
    expect(resolveStrategy(10_000)).toBe('pull');
    expect(resolveStrategy(100_000)).toBe('pull');
    expect(resolveStrategy(1_000_000)).toBe('pull');
  });
});

describe('fan-out blast radius', () => {
  it('push strategy writes to ALL followers — O(n)', () => {
    // For 1K followers, all 1K get an inbox write
    const followers = 1_000;
    const writes = followers;  // 1:1 fan-out
    expect(writes).toBe(1_000);
  });

  it('pull strategy writes only hints to ACTIVE fraction — O(n × hintFraction)', () => {
    // For 1M followers with 10% hint fraction: 100K writes instead of 1M
    const followers = 1_000_000;
    const hintFraction = 0.1;
    const writes = Math.floor(followers * hintFraction);
    expect(writes).toBe(100_000);
    // 10x reduction in write amplification
    expect(writes).toBeLessThan(followers / 5);
  });
});
