-- Migration 001: Core schema for the Hybrid Notification & Feed System

CREATE EXTENSION IF NOT EXISTS "uuid-ossp";
CREATE EXTENSION IF NOT EXISTS "pg_trgm";

CREATE TABLE IF NOT EXISTS users (
    id              UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
    username        VARCHAR(50) UNIQUE NOT NULL,
    display_name    VARCHAR(100) NOT NULL,
    bio             TEXT,
    avatar_url      TEXT,
    follower_count  INTEGER NOT NULL DEFAULT 0,
    following_count INTEGER NOT NULL DEFAULT 0,
    post_count      INTEGER NOT NULL DEFAULT 0,
    fanout_strategy VARCHAR(10) NOT NULL DEFAULT 'push',
    created_at      TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    updated_at      TIMESTAMPTZ NOT NULL DEFAULT NOW()
);
CREATE INDEX idx_users_username ON users(username);
CREATE INDEX idx_users_follower_count ON users(follower_count DESC);
CREATE INDEX idx_users_fanout_strategy ON users(fanout_strategy);

CREATE TABLE IF NOT EXISTS posts (
    id                       UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
    author_id                UUID NOT NULL REFERENCES users(id) ON DELETE CASCADE,
    content                  TEXT NOT NULL CHECK (char_length(content) <= 280),
    reply_to_id              UUID REFERENCES posts(id) ON DELETE SET NULL,
    like_count               INTEGER NOT NULL DEFAULT 0,
    reply_count              INTEGER NOT NULL DEFAULT 0,
    repost_count             INTEGER NOT NULL DEFAULT 0,
    fanout_complete          BOOLEAN NOT NULL DEFAULT FALSE,
    fanout_strategy          VARCHAR(10) NOT NULL DEFAULT 'push',
    author_followers_at_post INTEGER NOT NULL DEFAULT 0,
    created_at               TIMESTAMPTZ NOT NULL DEFAULT NOW()
);
CREATE INDEX idx_posts_author_id ON posts(author_id);
CREATE INDEX idx_posts_created_at ON posts(created_at DESC);
CREATE INDEX idx_posts_feed_pull ON posts(author_id, created_at DESC) WHERE fanout_strategy='pull';

CREATE TABLE IF NOT EXISTS follows (
    follower_id    UUID NOT NULL REFERENCES users(id) ON DELETE CASCADE,
    followee_id    UUID NOT NULL REFERENCES users(id) ON DELETE CASCADE,
    created_at     TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    last_active_at TIMESTAMPTZ,
    is_active      BOOLEAN NOT NULL DEFAULT TRUE,
    PRIMARY KEY (follower_id, followee_id)
);
CREATE INDEX idx_follows_followee_id ON follows(followee_id);
CREATE INDEX idx_follows_follower_id ON follows(follower_id);
CREATE INDEX idx_follows_active ON follows(followee_id, is_active) WHERE is_active=TRUE;

CREATE TABLE IF NOT EXISTS notifications (
    id           UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
    user_id      UUID NOT NULL REFERENCES users(id) ON DELETE CASCADE,
    type         VARCHAR(30) NOT NULL,
    actor_id     UUID REFERENCES users(id) ON DELETE SET NULL,
    post_id      UUID REFERENCES posts(id) ON DELETE SET NULL,
    payload      JSONB NOT NULL DEFAULT '{}',
    is_read      BOOLEAN NOT NULL DEFAULT FALSE,
    delivered_at TIMESTAMPTZ,
    created_at   TIMESTAMPTZ NOT NULL DEFAULT NOW()
);
CREATE INDEX idx_notifications_user_id ON notifications(user_id, created_at DESC);
CREATE INDEX idx_notifications_unread ON notifications(user_id, is_read) WHERE is_read=FALSE;

CREATE TABLE IF NOT EXISTS fanout_events (
    id            UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
    post_id       UUID NOT NULL REFERENCES posts(id) ON DELETE CASCADE,
    strategy      VARCHAR(10) NOT NULL,
    total_targets INTEGER NOT NULL DEFAULT 0,
    processed     INTEGER NOT NULL DEFAULT 0,
    failed        INTEGER NOT NULL DEFAULT 0,
    duration_ms   INTEGER,
    status        VARCHAR(20) NOT NULL DEFAULT 'pending',
    error_message TEXT,
    started_at    TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    completed_at  TIMESTAMPTZ
);
CREATE INDEX idx_fanout_events_post_id ON fanout_events(post_id);
CREATE INDEX idx_fanout_events_status ON fanout_events(status, started_at DESC);

-- Auto-update follower/following counts + fan-out strategy on follow/unfollow
CREATE OR REPLACE FUNCTION update_follow_counts() RETURNS TRIGGER AS $$
BEGIN
    IF TG_OP = 'INSERT' THEN
        UPDATE users SET follower_count=follower_count+1, updated_at=NOW() WHERE id=NEW.followee_id;
        UPDATE users SET following_count=following_count+1, updated_at=NOW() WHERE id=NEW.follower_id;
        UPDATE users SET fanout_strategy=CASE
            WHEN follower_count>=10000 THEN 'pull'
            WHEN follower_count>=1000  THEN 'hybrid'
            ELSE 'push' END WHERE id=NEW.followee_id;
    ELSIF TG_OP = 'DELETE' THEN
        UPDATE users SET follower_count=GREATEST(0,follower_count-1), updated_at=NOW() WHERE id=OLD.followee_id;
        UPDATE users SET following_count=GREATEST(0,following_count-1), updated_at=NOW() WHERE id=OLD.follower_id;
        UPDATE users SET fanout_strategy=CASE
            WHEN follower_count>=10000 THEN 'pull'
            WHEN follower_count>=1000  THEN 'hybrid'
            ELSE 'push' END WHERE id=OLD.followee_id;
    END IF;
    RETURN NULL;
END;
$$ LANGUAGE plpgsql;
CREATE TRIGGER trg_follow_counts AFTER INSERT OR DELETE ON follows FOR EACH ROW EXECUTE FUNCTION update_follow_counts();

CREATE OR REPLACE FUNCTION update_post_count() RETURNS TRIGGER AS $$
BEGIN
    IF TG_OP='INSERT' THEN UPDATE users SET post_count=post_count+1, updated_at=NOW() WHERE id=NEW.author_id;
    ELSIF TG_OP='DELETE' THEN UPDATE users SET post_count=GREATEST(0,post_count-1), updated_at=NOW() WHERE id=OLD.author_id;
    END IF;
    RETURN NULL;
END;
$$ LANGUAGE plpgsql;
CREATE TRIGGER trg_post_count AFTER INSERT OR DELETE ON posts FOR EACH ROW EXECUTE FUNCTION update_post_count();
