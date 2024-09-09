import { BROKER1, BROKER2, NETWORK, FILTERED_TXS_TOPIC, MONGODB_URI, CONTRACT_ADDRESS } from './config';

import { setLogger } from './logger';

const logger = setLogger('../logs/sink');

import { MongoClient } from 'mongodb';

const client = new MongoClient(MONGODB_URI);

import { Kafka } from 'kafkajs';

const kafka = new Kafka({
	clientId: 'sink',
	brokers: [BROKER1, BROKER2]
});

const consumer = kafka.consumer({ groupId: 'filtered-tx-consumer' });

const isInteger = (value) => {
    if (typeof value === 'number') {
      return null;
    }
    if (typeof value === 'string') {
      return value;
    }
};

const buildUTXO = (data, index, output) => {
    const doc = {
        status : 0,
        request: {
            blockHeight: data.blockHeight,
            blockHash: data.blockHash,
            blockSlot: data.blockSlot,	
            transactionId: data.id,
            outputIdx: index, 
            value: output.value,
            datumHash: isInteger(data.indicator),
            datum: data.decodedDatums[data.indicator],
            datumType: data.datumType,
            invBefore: null,
            invHereafter: null
        },
        response: {
            validityInterval: null,
            redeemers: null,
            transactionId: null,
            inputs: null,
            outputs: null,
            blockHeight: null,
            blockHash: null,
            blockSlot: null
        }
    }
    return doc;
}

const runConsumer = async () => {
	try {
        await client.connect();
        const db = client.db('cardano-oracle');
        const collection = db.collection(`filtered-utxos-${NETWORK}`);
		await consumer.connect();
		await consumer.subscribe({ topic: FILTERED_TXS_TOPIC, fromBeginning: true });
		await consumer.run({
			eachMessage: async ({ topic, partition, message }) => {
				try {
                    const data = JSON.parse(message.value);
                    logger.info(`Iterating over tx ${data.id} outputs.`);
                    for (const [index, output] of data.outputs.entries()) {
                        if (output.address == CONTRACT_ADDRESS) {
                            const utxo = buildUTXO(data, index, output);
                            await collection.insertOne(utxo);
                        }
                    }
					await consumer.commitOffsets([{ topic, partition, offset: message.offset + 1 }]);
				} catch (error) {
					logger.error(`Error processing message: ${error}`);
				}				
			},
		});
	} catch (error) {
		logger.error(`Error running: ${error}`);
	}
};

runConsumer().catch(console.error);

process.on('SIGINT', async () => {
    await consumer.disconnect();
    await client.close();
    process.exit(0);
})