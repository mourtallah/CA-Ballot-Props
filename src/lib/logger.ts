import winston from 'winston';
import { config } from '../config';

const { combine, timestamp, json, colorize, simple } = winston.format;

export const logger = winston.createLogger({
  level: config.env === 'production' ? 'info' : 'debug',
  format: config.env === 'production'
    ? combine(timestamp(), json())
    : combine(colorize(), timestamp({ format: 'HH:mm:ss' }), simple()),
  transports: [new winston.transports.Console()],
  defaultMeta: { service: 'notifeed', serverId: config.wsServerId },
});
