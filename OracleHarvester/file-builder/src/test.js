import { getContractsById, getSingleTransaction, getTransactionsForContract } from "./marloweFuncs";

const main = async () => {
    let contract = null;
    let transaction = null;
    let transactions = null;

    // successful examples
    contract = await getContractsById('ef4a30d7f3a2edcc22d4450a4e2b1cb8f192e7ea9e8a2982470e2b7bc8f96ea0', 0);
    console.log(contract.status, contract.statusText);
    transaction = await getSingleTransaction(
        '9f90643a708badd26fe7a9cc38cb62f207132c9563dacba119851bcf75d19b69', 0, 
        'e49012ed11b35245a881ed63fc8931c3e7fb0eee7a6c5e430c05688cff95f24a');
    console.log(transaction.status, transaction.statusText);
    transactions = await getTransactionsForContract('ef4a30d7f3a2edcc22d4450a4e2b1cb8f192e7ea9e8a2982470e2b7bc8f96ea0', 0);
    console.log(transactions.status, transactions.statusText);

    // examples with errors
    contract = await getContractsById('ff4a30d7f3a2edcc22d4450a4e2b1cb8f192e7ea9e8a2982470e2b7bc8f96ea0', 0);
    console.log(contract.status, contract.message);
    transaction = await getSingleTransaction(
        '9f90643a708badd26fe7a9cc38cb62f207132c9563dacba119851bcf75d19b69', 1, 
        'e49012ed11b35245a881ed63fc8931c3e7fb0eee7a6c5e430c05688cff95f24a');
    console.log(transaction.status, transaction.message);
    transactions = await getTransactionsForContract('ef4a30d7f3a2edcc22d4450a4e2b1cb8f192e7ea9e8a2982470e2b7bc8f96ea0', 1);
    console.log(transactions.status, transactions.message);
    return;
}

const main_test = async () => {
    const tic = performance.now();
    await main();
    const toc = performance.now();
    const executionTime = ((toc - tic)/1000.0)/60.0;
    console.log(executionTime)
}

if (require.main === module) {
    main_test();
}
