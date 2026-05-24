import { Router, Request, Response } from 'express';
import { z } from 'zod';
import * as userModel from '../../models/user.model';
import { query } from '../../db/client';
import type { ApiResponse, NotificationRow } from '../../types';

export const usersRouter = Router();

const CreateUserSchema = z.object({
  username: z.string().min(1).max(50).regex(/^[a-zA-Z0-9_]+$/),
  displayName: z.string().min(1).max(100),
  bio: z.string().max(160).optional(),
  avatarUrl: z.string().url().optional(),
});

// POST /users — register a user
usersRouter.post('/', async (req: Request, res: Response) => {
  try {
    const body = CreateUserSchema.parse(req.body);
    const existing = await userModel.findByUsername(body.username);
    if (existing) {
      return res.status(409).json({ success: false, error: 'Username taken' });
    }
    const user = await userModel.createUser(body);
    return res.status(201).json({ success: true, data: user });
  } catch (err) {
    if (err instanceof z.ZodError) {
      return res.status(400).json({ success: false, error: err.message });
    }
    return res.status(500).json({ success: false, error: String(err) });
  }
});

// GET /users/:id — get a user profile
usersRouter.get('/:id', async (req: Request, res: Response) => {
  try {
    const user = await userModel.findById(req.params.id);
    if (!user) return res.status(404).json({ success: false, error: 'User not found' });
    return res.json({ success: true, data: user });
  } catch (err) {
    return res.status(500).json({ success: false, error: String(err) });
  }
});

// POST /users/:id/follow — follow a user
usersRouter.post('/:id/follow', async (req: Request, res: Response) => {
  try {
    const followerId = req.headers['x-user-id'] as string || req.body.followerId;
    if (!followerId) return res.status(401).json({ success: false, error: 'Unauthorized' });

    await userModel.follow(followerId, req.params.id);
    return res.json({ success: true, data: { followed: req.params.id } });
  } catch (err) {
    return res.status(500).json({ success: false, error: String(err) });
  }
});

// DELETE /users/:id/follow — unfollow
usersRouter.delete('/:id/follow', async (req: Request, res: Response) => {
  try {
    const followerId = req.headers['x-user-id'] as string || req.body.followerId;
    if (!followerId) return res.status(401).json({ success: false, error: 'Unauthorized' });

    await userModel.unfollow(followerId, req.params.id);
    return res.json({ success: true, data: { unfollowed: req.params.id } });
  } catch (err) {
    return res.status(500).json({ success: false, error: String(err) });
  }
});

// GET /users/:id/notifications — get user notifications
usersRouter.get('/:id/notifications', async (req: Request, res: Response) => {
  try {
    const limit = parseInt(req.query.limit as string) || 20;
    const rows = await query<NotificationRow>(
      `SELECT * FROM notifications
       WHERE user_id = $1
       ORDER BY created_at DESC
       LIMIT $2`,
      [req.params.id, limit]
    );
    const notifications = rows.map(r => ({
      id: r.id,
      userId: r.user_id,
      type: r.type,
      actorId: r.actor_id,
      postId: r.post_id,
      payload: r.payload,
      isRead: r.is_read,
      deliveredAt: r.delivered_at,
      createdAt: r.created_at,
    }));
    return res.json({ success: true, data: notifications });
  } catch (err) {
    return res.status(500).json({ success: false, error: String(err) });
  }
});
