import { ROOT_DIR, ALLOWED_PAIRS, LOCAL_STORAGE_PATH, EXCHANGE_NAMES } from './config';
import { buildCommand, readMyFile, writeMyFile, my_exec } from './utils';
import { setLogger } from './logger';
import { candleSticksAggDis } from './ccdb';
import path from 'path';

const logger = setLogger('../logs/nextMarloweStep');

const isFloat = (num) => {
    return Number(num) === num && !Number.isInteger(num);
}

const numDecimalPlaces = (num) => {
    let numStr = num.toString();
    let decimalDot = numStr.indexOf('.');

    if (decimalDot === -1) {
        return 0;
    }

    const decimalPlaces = numStr.length - decimalDot - 1;

	if (decimalPlaces > 9) {
		return 9;
	}

	return decimalPlaces;
}

export const processNextMarloweStep = async (collection, document) => {

    const now = new Date();
    const fiveMinutesInMillis = 5 * 60 * 1000; 

    const invalidBeforePOSIX = now.getTime() - fiveMinutesInMillis;
    const invalidHereafterPOSIX = now.getTime() + fiveMinutesInMillis

    const invalidBefore = new Date(invalidBeforePOSIX);
    const invalidHereafter = new Date(invalidHereafterPOSIX);

    const marloweContract = document.request.datum.marloweContract;
    const marloweIndex = document.request.datum.marloweIndex;
    const TX_DIRECTORY = `${marloweContract}#${marloweIndex}`;
    const choiceName = document.request.datum.choiceToSolve;
	const dataTag = document.request.datum.dataTag;
    const choiceParty = 'addr_test1vqj8urdyuxkyh95x3v0yhu4eftddu8zcc324gzw396c28vgcwyugr';

	if (!ALLOWED_PAIRS.includes(dataTag)) {
		logger.warn(`${dataTag} is not in the list of allowed pairs.`);
		await collection.updateOne({_id:document._id}, {$set: {status: -2}});
        return 1;
	}

	const rangeDateEnd = new Date();
    rangeDateEnd.setSeconds(rangeDateEnd.getSeconds());
    const rangeDateStart = new Date(rangeDateEnd);
    rangeDateStart.setSeconds(rangeDateStart.getSeconds() - 300);

	const candle_sticks = await candleSticksAggDis(
		0, 5, 
		rangeDateStart.toISOString(), 
        rangeDateEnd.toISOString(),
		EXCHANGE_NAMES,
		[dataTag]
	);

	const candles = candle_sticks.data.pairs[0].candles;
	const aggregatedAveragePrice = candles[candles.length-1].aggregatedAveragePrice;

	let decimalPlaces = null;
	let factor = null;
	let scaledNumber = aggregatedAveragePrice;

	if (isFloat(aggregatedAveragePrice)) {
		decimalPlaces = numDecimalPlaces(aggregatedAveragePrice);
		factor = 10 ** decimalPlaces;
		scaledNumber = Math.floor(aggregatedAveragePrice * factor);
		logger.info(`Scaling ${aggregatedAveragePrice} with a factor of ${factor}.`);
	}

	const prepareCommandStr =`marlowe-cli run prepare \
	  --choice-name '${choiceName}' \
	  --choice-party ${choiceParty} \
	  --choice-number ${scaledNumber} \
	  --invalid-before ${invalidBeforePOSIX} \
	  --invalid-hereafter ${invalidHereafterPOSIX} \
	  --marlowe-file ${ROOT_DIR}/${TX_DIRECTORY}/tx-prev.marlowe \
	  --out-file ${ROOT_DIR}/${TX_DIRECTORY}/tx-next.marlowe`;
	
	let prepareCommand = buildCommand(prepareCommandStr);
	
	await my_exec(prepareCommand);
	
	const filePath = `${LOCAL_STORAGE_PATH}/${TX_DIRECTORY}/tx-next.marlowe`;
	const jsonData = readMyFile(filePath);
	
	const data = jsonData.tx.inputs[0];
	
	writeMyFile(data, path.join(LOCAL_STORAGE_PATH, TX_DIRECTORY, 'tx-next.inputs'));
	writeMyFile(jsonData.tx.contract, path.join(LOCAL_STORAGE_PATH, TX_DIRECTORY, 'tx-next.contract'));
	writeMyFile(jsonData.tx.state, path.join(LOCAL_STORAGE_PATH, TX_DIRECTORY, 'tx-next.state'));

	const redeemerCommandStr = `marlowe-cli contract redeemer \
	  --input-file ${ROOT_DIR}/${TX_DIRECTORY}/tx-next.inputs \
	  --out-file ${ROOT_DIR}/${TX_DIRECTORY}/marloweInput.redeemer`;
	
	const redeemerCommand = buildCommand(redeemerCommandStr);
	
	await my_exec(redeemerCommand);
	
	const datumContractStr = `marlowe-cli contract datum \
	  --contract-file ${ROOT_DIR}/${TX_DIRECTORY}/tx-next.contract \
	  --state-file ${ROOT_DIR}/${TX_DIRECTORY}/tx-next.state \
	  --out-file ${ROOT_DIR}/${TX_DIRECTORY}/marloweOutput.datum`;
	
	const datumContractCommand = buildCommand(datumContractStr);
	await my_exec(datumContractCommand);
    await collection.updateOne({_id:document._id}, {$set: { 'request.invBefore': invalidBefore,  'request.invHereafter': invalidHereafter}});
	await collection.updateOne({_id:document._id}, {$set: { status: 1}});
	return;
};