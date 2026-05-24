import { Router, Request, Response } from 'express';
import { z } from 'zod';
import * as postModel from '../../models/post.model';
import * as userModel from '../../models/user.model';
import { dispatchFanout, resolveStrategy } from '../../services/fanout/fanout.service';
import { assembleFeed } from '../../services/feed/feed.service';
import { logger } from '../../lib/logger';
import type { ApiResponse } from '../../types';

export const postsRouter = Router();

const CreatePostSchema = z.object({
  content: z.string().min(1).max(280),
  replyToId: z.string().uuid().optional(),
});

// POST /posts — create a new post
postsRouter.post('/', async (req: Request, res: Response) => {
  try {
    const body = CreatePostSchema.parse(req.body);
    // In a real app, get userId from auth middleware
    const authorId = (req.headers['x-user-id'] as string) || req.body.authorId;

    if (!authorId) {
      return res.status(401).json({ success: false, error: 'Unauthorized' });
    }

    const author = await userModel.findById(authorId);
    if (!author) {
      return res.status(404).json({ success: false, error: 'User not found' });
    }

    const strategy = resolveStrategy(author.followerCount);

    const post = await postModel.createPost({
      authorId,
      content: body.content,
      replyToId: body.replyToId,
      fanoutStrategy: strategy,
      authorFollowersAtPost: author.followerCount,
    });

    // Kick off async fan-out (non-blocking)
    dispatchFanout(post, author.followerCount).catch(err =>
      logger.error('Fan-out dispatch error', { postId: post.id, error: String(err) })
    );

    const response: ApiResponse = {
      success: true,
      data: post,
      meta: { fanoutStrategy: strategy, followerCount: author.followerCount },
    };
    return res.status(201).json(response);

  } catch (err) {
    if (err instanceof z.ZodError) {
      return res.status(400).json({ success: false, error: err.message });
    }
    logger.error('Create post error', { error: String(err) });
    return res.status(500).json({ success: false, error: 'Internal server error' });
  }
});

// GET /posts/:id — get a single post
postsRouter.get('/:id', async (req: Request, res: Response) => {
  try {
    const post = await postModel.findById(req.params.id);
    if (!post) return res.status(404).json({ success: false, error: 'Post not found' });
    return res.json({ success: true, data: post });
  } catch (err) {
    return res.status(500).json({ success: false, error: String(err) });
  }
});

// GET /posts/feed/:userId — assemble and return user's feed
postsRouter.get('/feed/:userId', async (req: Request, res: Response) => {
  try {
    const { userId } = req.params;
    const limit = parseInt(req.query.limit as string) || 20;
    const cursor = req.query.cursor as string | undefined;

    const feed = await assembleFeed(userId, { limit, cursor });
    return res.json({ success: true, data: feed });
  } catch (err) {
    logger.error('Feed assembly error', { error: String(err) });
    return res.status(500).json({ success: false, error: 'Feed assembly failed' });
  }
});
