import { setLogger } from './logger';
import { connectToMongo } from './db';
import { processCurrentMarloweStep } from './currentMarloweStep';
import { processNextMarloweStep } from './nextMarloweStep';
import { MONGODB_URI, NETWORK } from './config';

const logger = setLogger('../logs/app');

const sleep = async (seconds) => {
    return new Promise( (resolve) => setTimeout(resolve, seconds * 1000));
}

const main = async () => {
    const client = await connectToMongo(MONGODB_URI);
    const database = client.db('cardano-oracle');
    const collection = database.collection(`filtered-utxos-${NETWORK}`);

    const pipeline = [
        { $match: { status: 0 } },
        { $sort: { 'request.blockHeight': 1 } } 
    ];

    try {
        while (true) {

            const utxos = await collection.aggregate(pipeline).toArray();

            if (utxos.length > 0) {
                console.log(`Updating ${utxos.length} UTXOs.`);

                for (const [index, document] of utxos.entries()) {
                    console.log(`UTXO[${index}] has ${document.request.blockHeight} height`);
                    const status = await processCurrentMarloweStep(collection, document);
                    if (status == 1) {
                        continue;
                    }
                    await processNextMarloweStep(collection, document);
                }
            }

            console.log('Waiting 20 seconds for new blocks to update.');
            await sleep(20);
        }
    } catch (error) {
        logger.error(`Problem processing documents: ${error}`);
    }

}

main();
