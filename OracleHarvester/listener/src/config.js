import * as dotenv from 'dotenv';
import { join } from 'path';

dotenv.config({ path: join(__dirname, '../.env') });

export const SOCKET_ADDRESS = process.env.SOCKET_ADDRESS;
export const PIPELINE_LENGTH = parseInt(process.env.PIPELINE_LENGTH);
export const DISTANCE_FROM_TIP = parseInt(process.env.DISTANCE_FROM_TIP);
export const REFERENCE_BLOCK_HEIGHT = parseInt(process.env.REFERENCE_BLOCK_HEIGHT);
export const REFERENCE_BLOCK_SLOT = parseInt(process.env.REFERENCE_BLOCK_SLOT);
export const REFERENCE_BLOCK_ID = process.env.REFERENCE_BLOCK_ID;
export const BROKER1 = process.env.BROKER1;
export const BROKER2 = process.env.BROKER2;
export const NETWORK = process.env.NETWORK;
export const RAW_TXS_TOPIC = `rawTxs${NETWORK}`;
export const MONGODB_URI = process.env.MONGODB_URI;