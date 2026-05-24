import { query } from '../db/client';
import type { User, UserRow } from '../types';

function rowToUser(row: UserRow): User {
  return {
    id: row.id, username: row.username, displayName: row.display_name,
    bio: row.bio, avatarUrl: row.avatar_url,
    followerCount: row.follower_count, followingCount: row.following_count,
    postCount: row.post_count, fanoutStrategy: row.fanout_strategy,
    createdAt: row.created_at, updatedAt: row.updated_at,
  };
}

export async function findById(id: string): Promise<User | null> {
  const rows = await query<UserRow>('SELECT * FROM users WHERE id=$1', [id]);
  return rows[0] ? rowToUser(rows[0]) : null;
}

export async function findByUsername(username: string): Promise<User | null> {
  const rows = await query<UserRow>('SELECT * FROM users WHERE username=$1', [username]);
  return rows[0] ? rowToUser(rows[0]) : null;
}

export async function createUser(data: {
  username: string; displayName: string; bio?: string; avatarUrl?: string;
}): Promise<User> {
  const rows = await query<UserRow>(
    `INSERT INTO users (username,display_name,bio,avatar_url) VALUES ($1,$2,$3,$4) RETURNING *`,
    [data.username, data.displayName, data.bio ?? null, data.avatarUrl ?? null]
  );
  return rowToUser(rows[0]);
}

export async function getFollowerIds(
  userId: string,
  opts: { limit?: number; activeOnly?: boolean; offset?: number } = {}
): Promise<string[]> {
  const { limit = 10_000, activeOnly = false, offset = 0 } = opts;
  const activeFilter = activeOnly ? 'AND f.is_active=TRUE' : '';
  const rows = await query<{ follower_id: string }>(
    `SELECT f.follower_id FROM follows f
     WHERE f.followee_id=$1 ${activeFilter}
     ORDER BY f.created_at DESC LIMIT $2 OFFSET $3`,
    [userId, limit, offset]
  );
  return rows.map(r => r.follower_id);
}

export async function follow(followerId: string, followeeId: string): Promise<void> {
  await query(`INSERT INTO follows (follower_id,followee_id) VALUES ($1,$2) ON CONFLICT DO NOTHING`, [followerId, followeeId]);
}

export async function unfollow(followerId: string, followeeId: string): Promise<void> {
  await query('DELETE FROM follows WHERE follower_id=$1 AND followee_id=$2', [followerId, followeeId]);
}

export async function updateLastActive(userId: string): Promise<void> {
  await query(`UPDATE follows SET last_active_at=NOW(), is_active=TRUE WHERE follower_id=$1`, [userId]);
}
