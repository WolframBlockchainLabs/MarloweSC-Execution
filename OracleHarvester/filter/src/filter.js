import { setLogger } from './logger';

const logger = setLogger('../logs/tx-filter');

import { Kafka } from 'kafkajs';

import { BROKER1, BROKER2, CONTRACT_ADDRESS, RAW_TXS_TOPIC, FILTERED_TXS_TOPIC } from './config';

const kafka = new Kafka({
	clientId: 'txsFilter',
	brokers: [BROKER1, BROKER2]
});

const consumer = kafka.consumer({ groupId: 'rawTxsConsumer' });
const producer = kafka.producer();

const validateMessage = async (topic, partition, messageOffset, messageValue) => {
	const value = JSON.parse(messageValue); 
    for (const [index, output] of value.outputs.entries()) {
		if (output.address == CONTRACT_ADDRESS) {
			logger.info(`Tx: ${value.id} has ${CONTRACT_ADDRESS} in output ${index}.`);
            return true;
		}
	}
    return false;
}

const decodeDatums = (value) => {
	const decodedDatums = {};
	let field = null;
	let indicator = null;
	let datumType = null;
	for (const [index, output] of value.outputs.entries()) {
		if (Object.keys(output).includes('datum')) {
			logger.info(`Output ${index} has datum.`);
			field = output.datum;
			indicator = index;
			datumType = 'inline';
		}
		if (Object.keys(output).includes('datumHash')) {
			logger.info(`Output ${index} has datumHash.`);
			field = value.datums[output.datumHash];
			indicator = output.datumHash;
			datumType = 'embed';
		}
		if (field) {
			const datum_Buffer = Buffer.from(field, 'hex');
			const decoded_datum = cborSync.decode(datum_Buffer);
			decodedDatums[indicator] = {
				'marloweContract': decoded_datum[0][0].hexSlice(),
				'marloweIndex': decoded_datum[1],
				'transactionId': decoded_datum[2][0].hexSlice(),
				'transactionIndex': decoded_datum[3],
				'choiceToSolve': decoded_datum[4].toString(),
				'dataTag': decoded_datum[5].toString(),
				'deadline':  decoded_datum[6],
				'beneficiaryAfterDeadline':  decoded_datum[7].hexSlice(),
				'targetDate': new Date(decoded_datum[6])
			}
		}
	}
	value.decodedDatums = decodedDatums;
	value.datumType = datumType;
	value.indicator = indicator;
	return value;
}

let isValid = false;
import cborSync from 'cbor-sync';

const runConsumer = async () => {
	try {
		await producer.connect();
		await consumer.connect();
		await consumer.subscribe({ topic: RAW_TXS_TOPIC, fromBeginning: true });
		await consumer.run({
			eachMessage: async ({ topic, partition, message }) => {
				try {
					
					isValid = await validateMessage(
						topic,
						partition,
						message.offset,
						message.value
					);

					if (isValid) {
						let value = JSON.parse(message.value); 
						let valueDecoded = decodeDatums(value);
						await producer.send({
							topic: FILTERED_TXS_TOPIC,
							messages: [
								{ value: JSON.stringify(valueDecoded) }
							]
						});
						logger.info(`Sending tx ${value.id} to ${FILTERED_TXS_TOPIC} queue.`);
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