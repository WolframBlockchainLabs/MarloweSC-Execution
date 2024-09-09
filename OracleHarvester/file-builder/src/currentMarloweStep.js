import { getContractsById, getSingleTransaction, getTransactionsForContract } from './marloweFuncs';
import { buildCommand, my_exec, readMyFile, writeMyFile, createMyDir } from './utils';
import { LOCAL_STORAGE_PATH, ROOT_DIR } from './config';
import { setLogger } from './logger';

import path from 'path';

const logger = setLogger('../logs/currentMarloweStep');
const staticData = readMyFile('../marloweStatics/statics.marlowe');

const isInteger = (value) => {
    return Number.isInteger(value);
}

const isHexString = (str) => {
    return typeof str === 'string' && /^[0-9a-fA-F]+$/.test(str);
}

const stringToHex = (str) => {
    const buffer = Buffer.from(str, 'utf-8');
    return buffer.toString('hex');
}

export const processCurrentMarloweStep = async (collection, document) => {

    const marloweContract = document.request.datum.marloweContract;

    if (!isHexString(marloweContract)) {
        console.log('Marlowe Contract is not valid hex string.');
        await collection.updateOne({_id:document._id}, {$set: {status: -1}});
        return 1;
    }

    const marloweIndex = document.request.datum.marloweIndex;

    if (!isInteger(marloweIndex)) {
        console.log('Marlowe Index is not integer.');
        await collection.updateOne({_id:document._id}, {$set: {status: -1}});
        return 1;
    }

    if (marloweIndex < 0) {
        console.log('Marlowe Index is not positive.');
        await collection.updateOne({_id:document._id}, {$set: {status: -1}});
        return 1;
    }

    logger.info(`Processing current marlowe-document ${marloweContract}#${marloweIndex}`);

    let marloweFile = {
        'era': 'babbage',
        'plutusVersion':'PlutusScriptV2',
        'tx':{}
    }

    // Get current contract status
    const generalContract = await getContractsById(marloweContract, marloweIndex);

    if (generalContract.status == 404) {
        console.log(generalContract.message);
        await collection.updateOne({_id:document._id}, {$set: {status: -1}});
        return 1;
    }

    const currentMarlowe = generalContract.data.resource.currentContract;

    if (Object.keys(generalContract.data.resource).includes('currentContract') && (generalContract.data.resource.currentContract == null)) {
        console.log('Therer is no current contract.');
        await collection.updateOne({_id:document._id}, {$set: {status: -1}});
        return 1;
    };

    //Get last input for contract
    const transactionsForContract = await getTransactionsForContract(marloweContract, marloweIndex);

    if (transactionsForContract.status == 404) {
        console.log(transactionsForContract.message);
        await collection.updateOne({_id:document._id}, {$set: {status: -1}});
        return 1;
    }

    const transactionId0 = transactionsForContract.data.results[0].resource.transactionId;

    if (!isHexString(transactionId0)) {
        console.log('Transaction ID is not valid hex string.');
        await collection.updateOne({_id:document._id}, {$set: {status: -1}});
        return 1;
    }

    const particularTransaction0 = await getSingleTransaction(marloweContract, marloweIndex, transactionId0);

    if (particularTransaction0.status == 404) {
        console.log(particularTransaction0.message);
        await collection.updateOne({_id:document._id}, {$set: {status: -1}});
        return 1;
    }

    const currentInputs = particularTransaction0.data.resource.inputs;
    const invBefore = new Date(particularTransaction0.data.resource.invalidBefore);
    const invHereafter = new Date(particularTransaction0.data.resource.invalidHereafter);
    const slotInvBefore = (invBefore.getTime() - staticData.slotConfig.scSlotZeroTime) / staticData.slotConfig.scSlotLength;
    const slotInvHereafter = (invHereafter.getTime() - staticData.slotConfig.scSlotZeroTime) / staticData.slotConfig.scSlotLength;

    //Get contract State
    const currentState = generalContract.data.resource.state;
    const accounts = currentState.accounts;

    let marloweValue = 0;
    
    for (const [_, account] of accounts.entries()) {
        const [_, lovelaceAmount] = account;
        marloweValue += lovelaceAmount;
    }

    await collection.updateOne({_id:document._id}, {$set: {'request.marloweValue': {'ada':{'lovelace': marloweValue}}}});

    marloweFile.tx.continuations = []; //quasi-static (Pending)
    marloweFile.tx.contract = currentMarlowe; //dynamic (Solved)
    marloweFile.tx.inputs = currentInputs; //dynamic (Solved)
    marloweFile.tx.marloweValidator = staticData.marloweValidator; //static (Solved)
    marloweFile.tx.openRolesValidator = staticData.openRolesValidator; //static (Solved)
    marloweFile.tx.payments = []; //quasi-static (Pending)
    marloweFile.tx.range = [slotInvBefore,slotInvHereafter]; //dynamic (Solved)
    marloweFile.tx.roles = {'unCurrencySymbol': ''}; //quasi-static (Pending)
    marloweFile.tx.rolesValidator = staticData.rolesValidator; //static (Solved)
    marloweFile.tx.slotConfig = staticData.slotConfig; //static (Solved)
    marloweFile.tx.state = currentState; //dynamic (Solved)

    const TX_DIRECTORY = `${marloweContract}#${marloweIndex}`;
    const contractPath = createMyDir(path.join(LOCAL_STORAGE_PATH, `${TX_DIRECTORY}`));
    writeMyFile(marloweFile, `${contractPath}/tx-prev.marlowe`);
    writeMyFile(marloweFile.tx.contract, `${contractPath}/tx-prev.contract`); 
    writeMyFile(marloweFile.tx.state, `${contractPath}/tx-prev.state`);
    
    const contractDatumCommandStr = `marlowe-cli contract datum \
	  --contract-file ${ROOT_DIR}/${TX_DIRECTORY}/tx-prev.contract \
	  --state-file ${ROOT_DIR}/${TX_DIRECTORY}/tx-prev.state \
	  --out-file ${ROOT_DIR}/${TX_DIRECTORY}/marloweInput.datum`;
	
	let contractDatumCommand = buildCommand(contractDatumCommandStr);
	
	await my_exec(contractDatumCommand);

    const datumType = document.request.datumType
    
    if(datumType == "embed"){
        const wolframInputDatum = {
            "constructor":0,
            "fields":[
                {"constructor":0,"fields":[{"bytes":document.request.datum.marloweContract}]},
                {"int":document.request.datum.marloweIndex},
                {"constructor":0,"fields":[{"bytes":document.request.datum.transactionId}]},
                {"int":document.request.datum.transactionIndex},
                {"bytes": stringToHex(document.request.datum.choiceToSolve)},
                {"bytes": stringToHex(document.request.datum.dataTag)},
                {"int":document.request.datum.deadline},
                {"bytes":document.request.datum.beneficiaryAfterDeadline}
            ]
        }

        writeMyFile(wolframInputDatum, `${contractPath}/wolframInput.datum`);
    }

    writeMyFile({"constructor":0,"fields":[]}, `${contractPath}/wolframInput.redeemer`);

    logger.info(`Process successfull for marlowe-document ${TX_DIRECTORY}`);

    return 0;
}