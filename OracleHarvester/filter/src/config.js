import * as dotenv from 'dotenv';
import { join } from 'path';

dotenv.config({ path: join(__dirname, '../.env') });

export const BROKER1 = process.env.BROKER1;
export const BROKER2 = process.env.BROKER2;
export const CONTRACT_ADDRESS = process.env.CONTRACT_ADDRESS;
export const NETWORK = process.env.NETWORK;
export const RAW_TXS_TOPIC = `rawTxs${NETWORK}`;
export const FILTERED_TXS_TOPIC = `filteredTxs${NETWORK}`;
export const MONGODB_URI = process.env.MONGODB_URI;