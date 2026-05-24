# Trade-off Analysis: Push vs Pull vs Hybrid

## The Core Tension

Every notification system faces the same fundamental trade-off:

> **Write-time work** (push) vs **Read-time work** (pull)

Neither extreme scales for all use cases. The art is knowing *when* to switch.

---

## Fan-out on Write (Push)

**What it is**: When Alice posts, immediately write a reference to her post
into every follower's "inbox" (a sorted list, Redis ZSET, or inbox table).

**Time complexity**: O(followers) per write

**When it shines**:
- Small accounts (< 1K followers): cost is trivial
- DMs and group chats: small, bounded recipient lists
- Alerts and transactional notifications: latency-critical

**Where it breaks**:
- 1M-follower account posts → 1M Redis/DB writes synchronously
- Causes write storms: one viral post saturates the broker
- Storage cost scales linearly with engagement (everyone with 1K followers
  gets their inbox populated for every post they see)

**Real-world examples**: WhatsApp (small groups), SMS delivery, email delivery

---

## Fan-out on Read (Pull)

**What it is**: Don't write anything on post creation. When a user opens their
feed, query for recent posts from everyone they follow.

**Time complexity**: O(follows × posts_per_account) per read

**When it shines**:
- Celebrity/viral accounts: one canonical write serves all readers
- Infrequently-accessed feeds: no wasted writes for offline users
- Content that benefits from ranking at read time

**Where it breaks**:
- User follows 500 accounts → 500 DB queries per feed load
- Thundering herd: celebrity posts trigger mass simultaneous reads
- Higher latency (can't beat "already in your inbox")
- Complex to paginate and keep consistent

**Real-world examples**: RSS readers, classic blog aggregators

---

## Hybrid (This System)

**What it is**: Threshold-based routing:
- Small accounts → push to all followers
- Mid-tier (1K–10K) → push to *active* followers only
- Celebrity (10K+) → push lightweight "hint" to top active followers;
  everyone else assembles feed on read

**Key insight from Twitter/X**: The system classifies accounts as "celebrities"
and explicitly skips inbox pre-fill for them. On feed load, the system:
1. Reads your pre-filled inbox (push items)
2. Queries recent posts from celebrity follows (pull items)
3. Merges and ranks

**Blast radius comparison**:
```
Account with 1M followers:
  Push:   1M inbox writes per post (DISASTER)
  Pull:   0 writes, but 1M reads on next feed load (thundering herd)
  Hybrid: 10% × active% ≈ ~50K hint writes + on-demand pull for rest
          10× reduction in write amplification
```

---

## The "Active User" Optimization

A key refinement: don't push to *all* followers, only *active* ones.

**Why**: 40-60% of followers of any large account haven't opened the app
in 30+ days. Writing to their inboxes is pure waste.

**Implementation**: Track `last_active_at` per follow relationship.
Only push to followers active in the last 24h (configurable).

**Savings**: For an account with 100K followers but 30% daily actives:
- Without optimization: 100K writes
- With optimization: 30K writes (67% reduction)

---

## Broker Choice: Redis Streams vs Kafka

| Feature | Redis Streams | Kafka |
|---------|--------------|-------|
| Throughput | ~500K msgs/sec | ~2M msgs/sec |
| Latency | < 1ms | 5–50ms |
| Durability | AOF persistence | Replication factor |
| Consumer groups | Yes | Yes |
| Replay | Yes (XRANGE) | Yes (offset replay) |
| Complexity | Low | High |
| Cost | Redis instance | Separate Kafka cluster |

**Recommendation**: Start with Redis Streams. Migrate to Kafka at 10M+ events/day.

---

## The "Sequence Number" Reliability Pattern

Every delivered event gets a monotonically increasing sequence number per user.

```
Client remembers: "I last received seq=47"
Client disconnects for 2 minutes
Client reconnects: sends lastSeq=47
Server replays: seq=48, 49, 50, 51 (missed events)
Client deduplicates: ignores seq≤47 if somehow received again
```

This gives **exactly-once delivery semantics** at the application layer
on top of an **at-least-once** transport (Redis Streams with ACK).

---

## Graceful Degradation

When the system is under severe load:

1. **Rate-limit fan-out**: Cap at N writes/sec. Slower delivery, but no crash.
2. **Degrade to pull-only**: Disable inbox writes entirely. All feeds become pull-based.
3. **Sample celebrity hints**: Instead of 10% of active followers, reduce to 1%.
4. **Circuit-break WS delivery**: Fall back to polling if WS server is overwhelmed.

**The golden rule**: A degraded but functional system > a fast but crashed one.
