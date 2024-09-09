import { URLSearchParams } from 'url';
import { CCDB_BASEURL, EXCHANGE_NAMES } from './config';
import { setLogger } from './logger';
import axios from 'axios';

const logger = setLogger('../logs/ccdb');

const createQueryString = (params) => {
    const _params = new URLSearchParams();
    const keys = Object.keys(params);

    if (keys.includes('exchangeNames[]')) {
        const exchangeNames = params['exchangeNames[]'];
        delete params['exchangeNames[]'];
        for (const [_, exName] of exchangeNames.entries()) {
            _params.append('exchangeNames[]', exName);
        }
    }

    if (keys.includes('symbols[]')) {
        const symbols = params['symbols[]'];
        delete params['symbols[]'];
        for (const [_, symbol] of symbols.entries()) {
            _params.append('symbols[]', symbol);
        }
    }

    for (const [clave, valor] of Object.entries(params)) {
        _params.append(clave, valor);
    }

    const queryString = _params.toString();
    return queryString;
}

const _make_request = async (req_name, params) => {
    if (req_name === 'candle-sticks/aggregate-discrete') {
        params['timeframeMinutes'] = 1;
    }
    try {
        logger.info(`Making ${req_name} request`);
        const queryString = createQueryString(params);
        const url = `${CCDB_BASEURL}/${req_name}?${queryString}`;
        const response = await axios.request(url);
        logger.info(`Succesful response for ${req_name}`);
        console.log(response.config.url);
        return response;
    } catch (error) {
        logger.error(`${req_name}: ${error}`);
        return error.response;
    }
}

export const exchanges = async (offset, limit) => {
    const params = {
        offset: offset,
        limit: limit
    };
    const response = await _make_request('exchanges', params);
    if (response.status == 200) {
        return response.data;
    }
}

export const markets = async (offset, limit, exchanges, tokenSymbols) => {
    const params = {
        offset: offset,
        limit: limit,
        'exchangeNames[]': exchanges,
        tokenSymbols: tokenSymbols
    };
    const response = await _make_request('markets', params);
    if (response.status == 200) {
        return response.data;
    }
}

export const candleSticksAggDis = async (offset, limit, rangeDateStart, rangeDateEnd, exchangeNames, symbol) => {
    const params = {
        offset: offset,
        limit: limit,
        rangeDateStart: rangeDateStart,
        rangeDateEnd: rangeDateEnd,
        'exchangeNames[]': exchangeNames,
        'symbols[]': symbol
    };
    const response = await _make_request('candle-sticks/aggregate-discrete', params);
    if (response.status == 200) {
        return response.data;
    }
}

export const exchangeRates = async (offset, limit, rangeDateStart, rangeDateEnd, exchangeNames, symbol) => {
    const params = {
        offset: offset,
        limit: limit,
        rangeDateStart: rangeDateStart,
        rangeDateEnd: rangeDateEnd,
        'exchangeNames[]': exchangeNames,
        symbol: symbol
    };
    const response = await _make_request('exchange-rates', params);
    if (response.status == 200) {
        return response.data;
    }
}

const main = async () => {
    const offset = 0;
    const limit = 5;

    // the way dates are selected needs to be re-evaluated.
    const rangeDateEnd = new Date();
    const rangeDateStart = new Date(rangeDateEnd);
    rangeDateStart.setSeconds(rangeDateStart.getSeconds() - 300);

    let data = null;
    data = await exchanges(offset, 22);
    const exchangeNames = data.data.map(obj => obj.name);

    data = await markets(offset, limit, exchangeNames, 'ADA');
    
    const pairs = []
    for (let i = 0; i < data.data.length; i++) {
        pairs.push({
                symbol: data.data[i].symbol,
                exchange: data.data[i].exchange.name
            }            
        )
    }

    console.log(pairs);

    data = await candleSticksAggDis(
        offset, limit, 
        rangeDateStart.toISOString(), 
        rangeDateEnd.toISOString(), 
        exchangeNames, ['BTC/USDC']
    );
    console.log(data.data.pairs[0].candles);
    data = await candleSticksAggDis(
        offset, limit, 
        rangeDateStart.toISOString(), 
        rangeDateEnd.toISOString(), 
        exchangeNames, ['ADA/EUR']
    );
    console.log(data.data.pairs[0].candles);
    data = await candleSticksAggDis(
        offset, limit, 
        rangeDateStart.toISOString(), 
        rangeDateEnd.toISOString(), 
        EXCHANGE_NAMES, ['BTC/USDT']
    );
    const candles = data.data.pairs[0].candles;
    console.log(candles[candles.length-1].aggregatedAveragePrice);
    console.log(Number.isInteger(candles[candles.length-1].aggregatedAveragePrice));
}

const main_test = async () => {
    const tic = performance.now();
    await main();
    const toc = performance.now();
    const executionTime = ((toc - tic)/1000.0)/60.0;
    console.log(`Elapsed time: ${executionTime} minutes`);
}

if (require.main === module) {
    main_test();
}
