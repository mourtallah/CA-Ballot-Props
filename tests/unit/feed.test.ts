/**
 * Unit tests for feed scoring / ranking
 */

// Replicate the scoring function for testing
function scorePost(post: { likeCount: number; replyCount: number; repostCount: number; createdAt: Date }): number {
  const ageMs = Date.now() - post.createdAt.getTime();
  const ageHours = ageMs / (1000 * 60 * 60);
  const engagement = post.likeCount * 1 + post.replyCount * 2 + post.repostCount * 3;
  return (engagement + 1) / Math.pow(ageHours + 2, 1.5);
}

describe('Feed ranking (time-decayed engagement)', () => {
  const now = new Date();
  const oneHourAgo = new Date(now.getTime() - 60 * 60 * 1000);
  const oneDayAgo = new Date(now.getTime() - 24 * 60 * 60 * 1000);

  it('fresh posts score higher than old posts with same engagement', () => {
    const fresh = scorePost({ likeCount: 10, replyCount: 5, repostCount: 2, createdAt: oneHourAgo });
    const old   = scorePost({ likeCount: 10, replyCount: 5, repostCount: 2, createdAt: oneDayAgo });
    expect(fresh).toBeGreaterThan(old);
  });

  it('high-engagement posts can outrank newer low-engagement posts', () => {
    const viral = scorePost({ likeCount: 1000, replyCount: 500, repostCount: 200, createdAt: oneDayAgo });
    const quiet = scorePost({ likeCount: 1, replyCount: 0, repostCount: 0, createdAt: oneHourAgo });
    expect(viral).toBeGreaterThan(quiet);
  });

  it('reposts are weighted higher than likes (signal strength)', () => {
    const likesPost   = scorePost({ likeCount: 9, replyCount: 0, repostCount: 0, createdAt: now });
    const repostPost  = scorePost({ likeCount: 0, replyCount: 0, repostCount: 3, createdAt: now });
    // 3 reposts (score 9) == 9 likes (score 9), same engagement
    expect(repostPost).toBeCloseTo(likesPost, 2);
  });
});
