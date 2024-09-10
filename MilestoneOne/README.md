# Milestone One
## Introduction

This milestone outlines the operation of a specialized oracle contract designed to supply financial information for Marlowe contracts via a `Choice` Input. The process involves the request and response steps. Additionally, a timeout policy has been implemented for cases where the oracle cannot answer the petition.

## Notebook Execution

The notebook for Milestone 1 requires the installation of the Marlowe-starter-kit tools. Please refer to the installation instructions provided in the following link: [Marlowe-starter-kit Installation Guide](https://github.com/input-output-hk/marlowe-starter-kit/blob/main/docs/docker.md).

To run the notebook, it is necessary to create a `credentials` directory and place a `skey` file along with its corresponding address in a `.address` file within this directory.

Additionally, the `.plutus` contract file must be located at the same level as the Jupyter notebook. All other files are generated dynamically during the execution of the cells.

## Oracle Features

#### Deployment

The request is done by allocating the payment and providing information about the Marlowe UTXO through the following datum structure:

```haskell
oracleDatum = {
       marloweContract :: TxId,
       marloweIndex :: Integer, 
       transactionId :: TxId,
       transactionIndex :: Integer,
       choiceToSolve :: BuiltinByteString,
       dataTag :: BuiltinByteString,
       deadline:: POSIXTime,
       beneficiaryAfterDeadline:: PubKeyHash
}
```

Where `marloweContract` and `marloweIndex` points to the marlowe contractc awaiting for the choice. `transactionId` and `transactionIndex` identify the UTXO  to consume. The `choiceToSolve` field specifies the name of the choice within the Marlowe contract context, while `dataTag` is a code that identifies the specific information the user wants to retrieve from the price feed service. `deadline` and `beneficiaryAfterDeadline` are parameters for the Timeout Policy.

#### Constraints

1. The Marlowe contract must be present in the transaction context.
2. The transaction must resolve the choice whose name matches the `choiceToSolve` field in the datum.
3. The transaction must be executed before the specified `deadline`.

#### Payment Mechanism

When all conditions are met, the ADA locked within the contract is redirected to the oracle wallet, allowing the Marlowe contract to proceed to its next step.

#### Timeout Policy

If the oracle is unable to provide the requested price feed data, the timeout policy permits the `beneficiaryAfterDeadline` to claim the ADA held within the Oracle Contract at any time following the deadline. To claim it, the beneficiary must be specified as a "required signer" in the transaction that consumes the UTXO.