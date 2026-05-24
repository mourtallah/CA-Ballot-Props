export type FanoutStrategy = 'push' | 'pull' | 'hybrid';

export interface User {
  id: string;
  username: string;
  displayName: string;
  bio?: string;
  avatarUrl?: string;
  followerCount: number;
  followingCount: number;
  postCount: number;
  fanoutStrategy: FanoutStrategy;
  createdAt: Date;
  updatedAt: Date;
}

export interface Post {
  id: string;
  authorId: string;
  content: string;
  replyToId?: string;
  likeCount: number;
  replyCount: number;
  repostCount: number;
  fanoutComplete: boolean;
  fanoutStrategy: FanoutStrategy;
  authorFollowersAtPost: number;
  createdAt: Date;
  author?: Pick<User, 'id' | 'username' | 'displayName' | 'avatarUrl' | 'followerCount'>;
}

export interface FanoutJob {
  postId: string;
  authorId: string;
  authorFollowerCount: number;
  strategy: FanoutStrategy;
  createdAt: string;
  messageId?: string;
}

export interface FanoutResult {
  postId: string;
  strategy: FanoutStrategy;
  totalTargets: number;
  processed: number;
  failed: number;
  durationMs: number;
}

export type RealtimeEventType =
  | 'new_post'
  | 'new_hint'
  | 'notification'
  | 'ping'
  | 'ack'
  | 'replay_start'
  | 'replay_end';

export interface RealtimeEvent {
  type: RealtimeEventType;
  seq: number;
  userId: string;
  payload: unknown;
  timestamp: string;
}

export interface FeedItem {
  post: Post;
  source: 'inbox' | 'pull' | 'own';
  score?: number;
}

export interface FeedPage {
  items: FeedItem[];
  nextCursor?: string;
  hasMore: boolean;
}

export interface ApiResponse<T = unknown> {
  success: boolean;
  data?: T;
  error?: string;
  meta?: Record<string, unknown>;
}

export interface ClientConnectOptions {
  userId: string;
  lastSeq?: number;
}

export type NotificationType = 'new_post' | 'reply' | 'like' | 'follow' | 'hint';

export interface UserRow {
  id: string; username: string; display_name: string; bio?: string;
  avatar_url?: string; follower_count: number; following_count: number;
  post_count: number; fanout_strategy: FanoutStrategy;
  created_at: Date; updated_at: Date;
}

export interface PostRow {
  id: string; author_id: string; content: string; reply_to_id?: string;
  like_count: number; reply_count: number; repost_count: number;
  fanout_complete: boolean; fanout_strategy: FanoutStrategy;
  author_followers_at_post: number; created_at: Date;
  author_username?: string; author_display_name?: string;
  author_avatar_url?: string; author_follower_count?: number;
}

export interface NotificationRow {
  id: string; user_id: string; type: NotificationType;
  actor_id?: string; post_id?: string;
  payload: Record<string, unknown>; is_read: boolean;
  delivered_at?: Date; created_at: Date;
}
