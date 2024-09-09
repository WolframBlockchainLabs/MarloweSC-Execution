import * as dotenv from 'dotenv';
import { join } from 'path';

dotenv.config({ path: join(__dirname, '../.env') });

export const ALLOWED_PAIRS = [
    'ADA/USDC', 
    'USDC/ADA',
    'ADA/USDT', 
    'USDT/ADA',
    'BTC/USDT',
    'ETH/BTC',
    'ADA/BTC',
    'XRP/BTC',
    'XTZ/BTC'
]

export const EXCHANGE_NAMES = [
    "Binance",
    "Bitfinex",
    "Bitget",
    "Bitstamp",
    "Bybit",
    "Gate.io",
    "Gemini",
    "Kraken",
    "KuCoin",
    "OKX",
    "XRPL",
    "Minswap",
    "MuesliSwap",
    "SandaeSwap",
    "VyFinance",
    "WingRiders",
    "SushiSwap",
    "Uniswap_v2",
    "Uniswap_v3",
    "Plenty_DeFi",
    "QuipuSwap_Stableswap",
    "QuipuSwap_v2"
]

export const NETWORK = process.env.NETWORK;

export const CCDB_BASEURL = process.env.CCDB_BASEURL;

export const MARLOW_URL = process.env.MARLOW_URL;
export const API_KEY = process.env.API_KEY;
export const MONGODB_URI = process.env.MONGODB_URI;

export const LOCAL_STORAGE_PATH = process.env.LOCAL_STORAGE_PATH;

export const CONTAINER_NAME = process.env.CONTAINER_NAME;
export const ROOT_DIR = process.env.ROOT_DIR;