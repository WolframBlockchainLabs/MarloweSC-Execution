#!/usr/bin/env wolframscript
(*Import config variables*)
configFile  = Quiet[Check[Import["./config.json", "RawJSON"], $Failed]];

(*paclet and blockchain config*)
pacletPath = configFile["PACLET_PATH"];
PacletDirectoryLoad[pacletPath];
Needs["Blockchain`"];

$BlockchainBase = {"Cardano", "Testnet"};
Blockchain`$TemplateBase = configFile["LEONIDAS_API"];

Get["./Credentials.wls"];

Print[DateString[],": Starting transaction-builder process."];

(*Mongo config*)
Needs["MongoLink`"]
mongoURI = configFile["MONGO_URI"];
mongoPort = configFile["MONGO_PORT"];
dbName = configFile["DATABASE_NAME"];
collName = configFile["COLLECTION_NAME"];

delphosPath = configFile["DELPHOS_PATH"];

oracleAddress = configFile["ORACLE_ADDRESS"];
marloweApi = configFile["MARLOWE_API"];


client = MongoConnect[<|"Host" -> mongoURI, "Port" -> mongoPort|>];
db = client[dbName];
collection = db[collName];

pickDocsToSolve[collection_, query_:None, projection_:None] := Module[{returnDocs,intermediate},
    intermediate = MongoCollectionFind[collection,query,projection];
    returnDocs = MongoCursorToArray[intermediate];
    returnDocs
]

fromDocToIntermediate[mongoDoc_]:=Module[{localMongoDoc = mongoDoc, intermediateState},

    marloweContract = localMongoDoc["request"]["datum"]["marloweContract"];
    marloweIndex = localMongoDoc["request"]["datum"]["marloweIndex"];
    choiceUtxo =  localMongoDoc["request"]["datum"]["transactionId"];
    choiceIndex =  localMongoDoc["request"]["datum"]["transactionIndex"];
    oracleUtxo = localMongoDoc["request"]["transactionId"];
    oracleIndex = localMongoDoc["request"]["outputIdx"];
    oracleDatumMode = localMongoDoc["request"]["datumType"];

    marloweLovelace = localMongoDoc["request"]["marloweValue"];
    oracleLovelace = localMongoDoc["request"]["value"];
    invBef = localMongoDoc["request"]["invBefore"];
    invHer = localMongoDoc["request"]["invHereafter"];

    
    intermediateState = <|
        "_id"->localMongoDoc["_id"],
        "marloweContract"-><|"transactionId"->marloweContract,"index"->marloweIndex|>,
        "choiceUtxo"-><|"transactionId"->choiceUtxo,"index"->choiceIndex|>,
        "oracleUtxo" -> <|"transactionId"->oracleUtxo,"index"->oracleIndex, "mode"-> oracleDatumMode|>,
        "marloweValue" -> marloweLovelace,
        "oracleValue" -> oracleLovelace,
        "invBefore"-> invBef,
        "invHereafter"->invHer
    |>;
    intermediateState
]

declareInputs[
    marloweContract:<|"transactionId"->contractId_,"index"->contractIndex_|>,
    choiceUtxo:<|"transactionId"->transactionId_,"index"->transactionIndex_|>,
    oracleUtxo:<|"transactionId"->oracleTransactionId_,"index"->oracleTransactionIndex_, "mode"->oracleDatumMode_|>
    ] := Module[{returnList, marloweInput, oracleInput, walletInput},
    
    marloweInput = <|
        "TransactionID"->transactionId,
        "Index"-> transactionIndex, 
        "ReferenceScript" -> <|"TransactionID"->"6887579ed3f0e34e6f6949a0827b19b6d0ab7895e4e7f6049985548cc2a76095","Index"->1|>,
        "Datum" -> File[Evaluate[FileNameJoin[{delphosPath,contractId~~"#"~~ToString@contractIndex,"marloweInput.datum"}]]],
        "Redeemer" -> <|
            "Tag" -> "Spend",
            "Data" -> File[Evaluate[FileNameJoin[{delphosPath,contractId~~"#"~~ToString@contractIndex,"marloweInput.redeemer"}]]]
            |>
        |>;
    oracleInput = <|
        "TransactionID" -> oracleTransactionId , 
        "Index" -> oracleTransactionIndex, 
        "ReferenceScript" -> oracleReferenceScriptInfo["oracleReferenceScript"], 
        "Redeemer" -> <|
            "Tag" -> "Spend", 
            "Data" -> File[Evaluate[FileNameJoin[{delphosPath,contractId~~"#"~~ToString@contractIndex,"wolframInput.redeemer"}]]]
            |>
        |>;

    If[oracleDatumMode == "embed",
        AppendTo[oracleInput,"Datum" -> File[Evaluate[FileNameJoin[{delphosPath,contractId~~"#"~~ToString@contractIndex,"wolframInput.datum"}]]]];
    ];
    returnList = {marloweInput,oracleInput}
    ]

declareOutputs[
    marloweContract:<|"transactionId"->contractId_,"index"->contractIndex_|>, 
    marloweAmount:<|"ada"-><|"lovelace"->marloweLovelace_|>|>, 
    oracleAmount:<|"ada"-><|"lovelace"->oracleLovelace_|>|>
    ] := Module[ {returnList, marloweOutput, oracleOutput, walletOutput},
    
    marloweOutput =<|
        "Address" ->"addr_test1wpsz02qpp3245nwkkzyg9wye7je3vlrwg5jqgufjyqkanpqef22lm",
        "Amount" -> Quantity[marloweLovelace,"CardanoLovelace"],
        "Datum" -> <|
            "Mode" -> "Embed",
            "Data" -> File[Evaluate[FileNameJoin[{delphosPath,contractId~~"#"~~ToString@contractIndex,"marloweOutput.datum"}]]]
            |>
        |>;
    oracleOutput = <|
        "Address" -> oracleAddress,
        "Amount" -> Automatic
        |>;
    
    returnList = {marloweOutput, oracleOutput}
    ]

docToTxStatement[intermediateState_]:= Module[{localDoc = intermediateState,processedDoc, inputs, outputs, collateralInput,collateralOutput,requiredSigner,validityInterval},

    docId = localDoc["_id"];
    oracleUtxo = localDoc["oracleUtxo"];
    choiceUtxo = localDoc["choiceUtxo"];
    marloweContract = localDoc["marloweContract"];

    marloweValue = localDoc["marloweValue"];
    oracleValue = localDoc["oracleValue"];

    dateInit = localDoc["invBefore"];
    dateEnd = localDoc["invHereafter"];

    inputs = declareInputs[marloweContract, choiceUtxo, oracleUtxo];
    outputs = declareOutputs[marloweContract, marloweValue, oracleValue];

    collateralInput = First[cardanoAddressUtxoList];
    collateralOutput = <|"Address"->oracleAddress, "Amount"->Automatic|>;

    requiredSigner = {oracleAddress};
    validityInterval = DateInterval[{dateInit,dateEnd}];

    processedDoc = <|
        "_id"->docId,
        "marloweContract"-> marloweContract,
        "statement" -> <|
            "Inputs" -> inputs,
            "Outputs" -> outputs,
            "CollateralInputs" -> collateralInput,
            "CollateralOutputs" -> collateralOutput,
            "RequiredSigners" -> requiredSigner,
            "ValidityInterval" -> validityInterval
        |>
    |>;
    processedDoc
]

unpackIn[input_] := Module[{}, <|"transactionId" -> input["TransactionID"], "index" -> input["Index"]|>]
unpackOut[output_] := Module[{}, <|"address" -> output["Address"], "amount" -> <|"ada" -> <|"lovelace" -> QuantityMagnitude[output["Amount"]]|>|>|>]

mongoUpdateFunc[collection_, txStatement_, result_BlockchainTransaction] := 
 Module[
    {updateObject, valInterval, redeemers, txId, ins, outs},
    valInterval = result["ValidityInterval"];
    redeemers = Import[File[Evaluate[FileNameJoin[{delphosPath,txStatement["marloweContract"]["transactionId"] ~~ "#" ~~ ToString@txStatement["marloweContract"]["index"],"tx-next.inputs"}]]], "RawJSON"];
    txId = result["TransactionID"];
    ins = Map[unpackIn, result["Inputs"]];
    outs = Map[unpackOut, result["Outputs"]];
    updateObject = <|
        "$set" -> <|
            "response.validityInteval" -> valInterval,
            "response.redeemers" -> redeemers,
            "response.transactionId" -> txId,
            "response.inputs" -> ins,
            "response.outputs" -> outs,
            "response.blockHeight" -> Null,
            "response.blockHash" -> Null,
            "response.blockSlot" -> Null
        |>
    |>;
    MongoCollectionUpdateOne[collection, <|"_id" -> txStatement["_id"]|>, updateObject]
  ]

mainFunction[] := Module[{docsToSolve,intermediateState, txStatement, result,stepOne,stepTwo,stepThree},

    docsToSolve = pickDocsToSolve[collection, <|"status"->1|>,<|"request"-><|"blockHeight"-> 0,"blockHash"->0,"blockSlot"->0|>|>];
    Print[DateString[],": "<>ToString@Length[docsToSolve]<>" contracts found in DB to be solved"];

    If[Length@docsToSolve>0,
        Do[
            intermediateState = fromDocToIntermediate[i];
            txStatement = docToTxStatement[intermediateState];
            result = Catch[
                stepOne=Quiet[BlockchainTransaction[txStatement["statement"]],{BlockchainTransaction::serialdelay}];
                If[MatchQ[stepOne,$Failed],Print[DateString[],": Error creating transaction for marloweContract "<>txStatement["marloweContract"]["transactionId"]<>"."];Throw[$Failed]];
                stepTwo = BlockchainTransactionSign[stepOne,cardanoKeys["PrivateKey"]];
                If[MatchQ[stepTwo,$Failed],Print[DateString[],": Error signing transaction for marloweContract "<>txStatement["marloweContract"]["transactionId"]<>"."];Throw[$Failed]];
                stepThree = BlockchainTransactionSubmit[stepTwo];
                If[MatchQ[stepThree,$Failed],Print[DateString[],": Error submitting transaction for marloweContract "<>txStatement["marloweContract"]["transactionId"]<>"."];Throw[$Failed]];
                stepThree
            ];

            If[MatchQ[result,_BlockchainTransaction],
                mongoUpdateFunc[collection, txStatement,result];
                Print[DateString[],": Status updated in DB for marloweContract "<>txStatement["marloweContract"]["transactionId"]<>"."]
                ,
                MongoCollectionUpdateOne[collection, <|"_id"->txStatement["_id"]|>,<|"$set"-><|"status"-> -5|>|>];
                Print[DateString[],": Error written in DB for marloweContract "<>txStatement["marloweContract"]["transactionId"]<>"."];
            ]

        ,
        {i,docsToSolve}
        ],
        Print[DateString[Now], ": No transactions to execute"];
    ]

];

mainFunction[];

Quit[];