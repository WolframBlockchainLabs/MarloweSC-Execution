import { MongoClient } from 'mongodb';
import { setLogger } from './logger';

const logger = setLogger('../logs/mongo');

export const connectToMongo = async (uri) => {
    const client = new MongoClient(uri)
    try {
        await client.connect();
        logger.info('Connected to MongoDB');
        return client;
    } catch (error) {
        logger.error(`Problem connecting to MongoDB: ${error}`);
    }
}

export const disconnectFromMongo = async (client) => {
    try {
        await client.close();
        logger.info('Disconnected from MongoDB');
        return;
    } catch (error) {
        logger.error('Error disconnecting from MongoDB:', error);
    }
}