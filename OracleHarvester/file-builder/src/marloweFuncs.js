import { setLogger } from './logger';

const logger = setLogger('../logs/demeter');

import { MARLOW_URL, API_KEY } from './config';

import axios from 'axios';

const url = MARLOW_URL;  

const _make_request = async (req_name, config) => {
    try {
        logger.info(`Making ${req_name} request`);
        const response = await axios.request(config);
        logger.info(`Succesful response for ${req_name}`);
        return response;
    } catch (error) {
        logger.error(`${req_name}: ${error}`);
        return error.response;
    }
}

export const getContracts = async () => {
    let config = {
        method: 'get',
        maxBodyLength: 1,
        url: url+'/contracts',
        headers: {
            'Accept': 'application/json;charset=utf-8',
            'dmtr-api-key': API_KEY
        }
    };
    await _make_request('getContracts', config);
}

export const getContractsById = async (hash, idx) => {
    let config = {
        method: 'get',
        maxBodyLength: 1,
        url: `${url}/contracts/${hash}%23${idx}`,
        headers: {
            'Accept': 'application/json;charset=utf-8',
            'dmtr-api-key': API_KEY
        }
    };
    
    const response = await _make_request('getContractsById', config);
    if (response.status == 200) {
        return response;
    }
    if (response.status == 404) {
        return {
            'status': response.status,
            'message': `${hash}#${idx} not found`
        }
    }
}

export const getContractSourceById = async (contractSourceId) => {
    let config = {
        method: 'get',
        maxBodyLength: Infinity,
        url: `${url}/contracts/sources/${contractSourceId}`,
        headers: {
            'Accept': 'application/json;charset=utf-8',
            'dmtr-api-key': API_KEY
        }
    };
    
    await _make_request('getContractSourceById', config);
}

export const getNextStepsForContract = async (hash, idx, validityStart, validityEnd) => {
    let config = {
        method: 'get',
        maxBodyLength: 1,
        url: `${url}/contracts/${hash}%23${idx}/next?validityStart=${validityStart}&validityEnd=${validityEnd}`,
        headers: {
            'Accept': 'application/json;charset=utf-8',
            'dmtr-api-key': API_KEY
        }
    };
    
    await _make_request('getNextStepsForContract', config);
}

export const getTransactionsForContract = async (hash, idx) => {
    let config = {
        method: 'get',
        maxBodyLength: 1,
        url: `${url}/contracts/${hash}%23${idx}/transactions`,
        headers: {
            'Accept': 'application/json;charset=utf-8',
            'dmtr-api-key': API_KEY
        }
    };
    
    const response = await _make_request('getTransactionsForContract', config);
    if (response.status == 206) {
        return response;
    }
    if (response.status == 404) {
        return {
            'status': response.status,
            'message': `${hash}#${idx} not found`
        }
    }
}

export const getSingleTransaction = async (cHash, cIdx, hash) => {
    let config = {
        method: 'get',
        maxBodyLength: 1,
        url: `${url}/contracts/${cHash}%23${cIdx}/transactions/${hash}`,
        headers: {
            'Accept': 'application/json;charset=utf-8',
            'dmtr-api-key': API_KEY
        }
    };
    
    const response = await _make_request('getContractTransactionById', config);
    if (response.status == 200) {
        return response;
    }
    if (response.status == 404) {
        return {
            'status': response.status,
            'message': `${cHash}#${cIdx} or ${hash} not found`
        };
    }
}