import { setLogger } from './logger';

const logger = setLogger('../logs/txs-listener');

import { SOCKET_ADDRESS, PIPELINE_LENGTH, DISTANCE_FROM_TIP, REFERENCE_BLOCK_HEIGHT, REFERENCE_BLOCK_SLOT, REFERENCE_BLOCK_ID, BROKER1, BROKER2, RAW_TXS_TOPIC, MONGODB_URI, NETWORK } from './config';

import { MongoClient } from 'mongodb';

const client = new MongoClient(MONGODB_URI);

import { Kafka } from 'kafkajs';

const kafka = new Kafka({
    clientId: 'tx-listener',
    brokers: [BROKER1,  BROKER2]
  });
  
const producer = kafka.producer();

const produceMessage = async (message) => {
    await producer.send({
        topic: RAW_TXS_TOPIC,
        messages: [
            { value: message }
        ]
    });
};

import WebSocket from 'ws';

const findIntersection = (id, point = 'origin') => JSON.stringify({
    'jsonrpc': '2.0',
    'method': 'findIntersection',
    'params': { 'points': [point] },
    'id': id
});

const nextBlock = (id) => JSON.stringify({
    'jsonrpc': '2.0',
    'method': 'nextBlock',
    'id': id
});

let responses = [];
let pipeline_tip = null;
let stopSync = false;
let lastBlock = null;
let pipeline_length = PIPELINE_LENGTH;

const cardanoSync = async (url) => {
    return new Promise( async (resolve, reject) => {

        await client.connect();
        const db = client.db('cardano-oracle');
        const collection = db.collection(`check-point`);
        const check_point = await collection.findOne({network: NETWORK});

        let slot = REFERENCE_BLOCK_SLOT;
        let id = REFERENCE_BLOCK_ID;

        if (!check_point) {
            logger.info('Inserting first check point.');
            const data = {
                'network': NETWORK,
                'height': REFERENCE_BLOCK_HEIGHT,
                'slot': REFERENCE_BLOCK_SLOT,
                'id': REFERENCE_BLOCK_ID
            };
            await collection.insertOne(data);
        } else {
            slot = check_point.slot;
            id = check_point.id;
        }

        await producer.connect();

        const socket = new WebSocket(url);

        socket.on('open', async () => {
            logger.info('Starting Synchronization.');
            socket.send(findIntersection(0, 
                {
                    slot: slot,
                    id: id
                }
            ));
            for (let i = 1; i < pipeline_length; i++) {
                socket.send(nextBlock(i));
            }
        });

        socket.on('close', async () => {
            logger.info('Synchronization ended');
            await producer.disconnect();
            resolve();
        });

        socket.on('error', (error) => {
            logger.error(error);
            reject(error);
        });

        socket.on('message', async (message) => {
            const response = JSON.parse(message);
            responses.push(response);
    
            if (response.id === (pipeline_length - 1)) {
                pipeline_tip = responses[responses.length-1].result.tip;
        
                for (let i = 0; i < responses.length; i++) {
                    const keysFromResult = Object.keys(responses[i].result);
                    if (!keysFromResult.includes('block')) {
                        logger.warn('Response with no block.');
                        continue;
                    }
        
                    const block = responses[i].result.block;
    
                    const keysFromBlock = Object.keys(block);
    
                    if (!keysFromBlock.includes('transactions')) {
                        logger.info(`Block ${block.height} doesn't have the transactions field.`);
                        continue;
                    }
    
                    if (block.transactions.length == 0) {
                        logger.info(`Block ${block.height} doesn't have transactions.`);
                        continue;
                    }
    
                    if (block.height >= pipeline_tip.height - pipeline_length) {
                        logger.info(`Starting to hold. Block ${block.height} is close to the tip.`);
                        stopSync = true;
                        break;
                    }
    
                    logger.info(`Block ${block.height} has ${block.transactions.length} transactions.`);
                    
                    for (const [index, tx] of block.transactions.entries()){
                        logger.info(`Sending tx[${index}]:${tx.id} to ${RAW_TXS_TOPIC} queue.`);
                        tx.blockHeight = block.height;
                        tx.blockSlot = block.slot;
                        tx.blockHash = block.id;
                        await produceMessage(JSON.stringify(tx)); 
                    }

                    await collection.updateOne(
                        {
                            network:NETWORK
                        }, 
                        {
                            $set: { 
                                height: block.height, 
                                slot: block.slot, 
                                id: block.id 
                            }
                        }
                    )

                }

                if (stopSync) {
                    socket.close();
                    return;
                }

                for (let i = 0; i < pipeline_length; i += 1) {
                    socket.send(nextBlock(i));
                }

                responses = [];
            }
    
        });
    })   
}

let ignore = true;
let tip = null;

const cardanoRealTime = async (url) => {
    return new Promise( async (resolve, reject) => {

        await client.connect();
        const db = client.db('cardano-oracle');
        const collection = db.collection(`check-point`);
        const check_point = await collection.findOne({network: NETWORK});

        let slot = REFERENCE_BLOCK_SLOT;
        let id = REFERENCE_BLOCK_ID;

        if (!check_point) {
            logger.info('Inserting first check point.');
            const data = {
                'network': NETWORK,
                'height': REFERENCE_BLOCK_HEIGHT,
                'slot': REFERENCE_BLOCK_SLOT,
                'id': REFERENCE_BLOCK_ID
            };
            await collection.insertOne(data);
        } else {
            slot = check_point.slot;
            id = check_point.id;
        }

        await producer.connect();

        const socket = new WebSocket(url);

        socket.on('open', async () => {
            logger.info('Starting Real Time operation.');
            socket.send(findIntersection(0, { slot: slot, id: id }));
            socket.send(nextBlock());
            socket.send(nextBlock(1));
        });

        socket.on('close', () => {
            resolve();
        });

        socket.on('error', (error) => {
            logger.error(error);
            reject(error);
        });

        socket.on('message', async (message) => {
            const response = JSON.parse(message);
            if (response.id == 0) {
                tip = response.result.tip.height;
                if (ignore) {
                    ignore = false;
                    return;
                }
                if (lastBlock.height < tip - DISTANCE_FROM_TIP) {
                    logger.info(`Response ID: ${response.id}.`);
                    logger.info(`Chain TIP: ${tip}.`);
                    logger.info(`Last block height: ${lastBlock.height}.`);
                    socket.send(nextBlock(1));
                    return;
                }
                socket.send(findIntersection(0, {slot:lastBlock.slot, id:lastBlock.id}));
                return;
            }
            if (response.id == 1) {
                logger.info(`Result direction: ${response.result.direction}.`);
                if (response.result.direction == 'backward') {
                    const point = response.result.point;
                    ignore = true;
                    socket.send(findIntersection(0, point));
                    socket.send(nextBlock());
                    socket.send(nextBlock(1));
                    return;
                }
                lastBlock = response.result.block;
                const lastBlockKeys = Object.keys(lastBlock);
                if (!lastBlockKeys.includes('transactions')) {
                    logger.info(`Block ${lastBlock.height} doesn't have the transactions field.`);
                }
                if (lastBlock.transactions.length == 0) {
                    logger.info(`Block ${lastBlock.height} doesn't have transactions.`);
                }
                if (lastBlock.transactions.length > 0) {
                    logger.info(`Block ${lastBlock.height} have transactions ${lastBlock.transactions.length} transactions.`);
                    for (const [index, tx] of lastBlock.transactions.entries()){
                        logger.info(`Sending tx[${index}]:${tx.id} to ${RAW_TXS_TOPIC} queue.`);
                        tx.blockHeight = lastBlock.height;
                        tx.blockSlot = lastBlock.slot;
                        tx.blockHash = lastBlock.id;
                        await produceMessage(JSON.stringify(tx)); 
                    }
                }

                await collection.updateOne(
                    {
                        network:NETWORK
                    }, 
                    {
                        $set: { 
                            height: lastBlock.height, 
                            slot: lastBlock.slot, 
                            id: lastBlock.id 
                        }
                    }
                )

                if (lastBlock.height < tip - DISTANCE_FROM_TIP) {
                    logger.info(`Response ID: ${response.id}.`);
                    logger.info(`Chain TIP: ${tip}.`);
                    logger.info(`Last block height: ${lastBlock.height}.`);
                    socket.send(nextBlock(1));
                    return;
                }

                logger.info(`Request findIntersection to: ${lastBlock.slot}, ${lastBlock.id}.`);
                socket.send(findIntersection(0, {slot:lastBlock.slot, id:lastBlock.id}));
                return;
            }
        });
    });
}

const main = async () => {
    await cardanoSync(SOCKET_ADDRESS);
    await cardanoRealTime(SOCKET_ADDRESS);
}

main()
