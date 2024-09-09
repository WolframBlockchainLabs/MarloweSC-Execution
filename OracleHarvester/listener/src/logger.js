import path from 'path';
import winston from 'winston';
import DailyRotateFile from 'winston-daily-rotate-file';

export const setLogger = (logs_path) => {
    const logsDirectory = path.resolve(__dirname, logs_path);

    const logFormat = winston.format.printf(({ level, message, timestamp }) => {
        return `${timestamp} ${level.toUpperCase()}: ${message}`;
    });
    
    const excludeErrorLevel = winston.format((info) => {
        if(info.level === 'error'){
            return false; 
        }
        return info; 
    });
    
    const infoLogFormat = winston.format.combine(
        winston.format.timestamp(),
        winston.format.splat(),
        excludeErrorLevel(), 
        logFormat
    );
    
    const errorLogFormat = winston.format.combine(
        winston.format.timestamp(),
        winston.format.splat(),
        logFormat
    );
    
    const errorTransport = new DailyRotateFile({
        filename: `${logsDirectory}/error-%DATE%.log`,
        datePattern: 'YYYY-MM-DD',
        level: 'error',
        zippedArchive: false,
        maxSize: '5m',
        maxFiles: null,
        format: errorLogFormat
    });
    
    const combinedTransport = new DailyRotateFile({
        filename: `${logsDirectory}/combined-%DATE%.log`,
        datePattern: 'YYYY-MM-DD',
        level: 'info',
        zippedArchive: false,
        maxSize: '5m',
        maxFiles: null,
        format: infoLogFormat
    });
    
    const logger = winston.createLogger({
        transports: [
            errorTransport,
            combinedTransport
        ]
    });
    
    return logger;
}