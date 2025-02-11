{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE ViewPatterns          #-}
{-# LANGUAGE NamedFieldPuns        #-}

module OracleValidator where

import           GHC.Generics                         (Generic)
import qualified Prelude                              as P (Show (..), Eq(..), IO, print)
import qualified PlutusTx
import qualified PlutusTx.Builtins                    as Builtins
import           PlutusTx.Prelude                     hiding (Semigroup (..), unless)
import           Ledger                               (Validator)--hiding (singleton, txInfoSignatores, txInfoValidRange)
import           Ledger.Typed.Scripts                 as Scripts
import qualified PlutusLedgerApi.V1                   as PlutusV1
import qualified PlutusLedgerApi.V1.Scripts           as Script
import           PlutusLedgerApi.V1.Interval          (contains)
import qualified PlutusLedgerApi.V2                   as PlutusV2
import           PlutusLedgerApi.V2.Contexts          as Contexts (ScriptPurpose(..))
import qualified Plutus.Script.Utils.Typed            as PSU
import qualified Plutus.Script.Utils.V2.Typed.Scripts as PSU.V2
import           PlutusTx.AssocMap
import qualified Plutus.Script.Utils.Scripts          as PSU.Scripts

import qualified MinimalTypes as MTypes

-----------------------------------------------
-- Defining Datum.
-----------------------------------------------
data WolframOracleDatum = WolframOracleDatum
     { marloweTx :: PlutusV2.TxOutRef,
       choiceName :: Builtins.BuiltinByteString,
       dataTag :: Builtins.BuiltinByteString,
       deadline :: PlutusV2.POSIXTime,
       beneficiary :: PlutusV2.PubKeyHash
     } deriving (P.Show, P.Eq)

instance Eq WolframOracleDatum where
    (==) = (==)

PlutusTx.unstableMakeIsData ''WolframOracleDatum

-----------------------------------------------
-- Defining Redeemer.
-----------------------------------------------
data WolframOracleRedeemer 
    = Execute
    | Reclaim
    deriving (P.Show, P.Eq)

instance Eq WolframOracleRedeemer where
    (==) = (==)

PlutusTx.unstableMakeIsData ''WolframOracleRedeemer

-----------------------------------------------
-- Defining custom ScriptContext
-----------------------------------------------
data WolframTxInfo = WolframTxInfo 
    {
        txInfoInputs :: BuiltinData,
        txInfoReferenceInputs :: BuiltinData,
        txInfoOutputs :: BuiltinData,
        txInfoFee :: BuiltinData,
        txInfoMint :: BuiltinData,
        txInfoTxCerts :: BuiltinData,
        txInfoWdrl :: BuiltinData,
        txInfoValidRange :: PlutusV1.POSIXTimeRange,
        txInfoSignatories :: [PlutusV1.PubKeyHash],
        txInfoRedeemers :: Map Contexts.ScriptPurpose Script.Redeemer,
        txInfoData :: BuiltinData,
        txInfoId :: BuiltinData,
        txInfoVotes :: BuiltinData,
        txInfoProposalProcedures :: BuiltinData,
        txInfoCurrentTreasuryAmount :: BuiltinData,
        txInfoTreasuryDonation :: BuiltinData
    }
    deriving(P.Eq)

instance Eq WolframTxInfo where
    (==) = (==)

PlutusTx.unstableMakeIsData ''WolframTxInfo


data WolframScriptContext = WolframScriptContext
    {
        scriptContextTxInfo :: WolframTxInfo,
        scriptContextPurpose:: BuiltinData
    }
    deriving(P.Eq)

instance Eq WolframScriptContext where
    (==) = (==)
PlutusTx.unstableMakeIsData ''WolframScriptContext
-----------------------------------------------
-- Defining Marlowe expected redeemer
-----------------------------------------------

type ExpectedRedeemerStructure = [MTypes.Input]

--------------------------------------------------------
--
--Wolfram Oracle Validator
--
--------------------------------------------------------
-----------------------------------------------
-- Defining on-chain validator for oracle.
-----------------------------------------------
{-# INLINABLE mkOracleFinalValidator #-}
mkOracleFinalValidator ::WolframOracleDatum -> WolframOracleRedeemer -> WolframScriptContext -> Bool
mkOracleFinalValidator WolframOracleDatum {beneficiary, deadline} Reclaim (WolframScriptContext WolframTxInfo {txInfoSignatories, txInfoValidRange} _) = 
    (&&)
      (traceIfFalse "The transaction wasn't signed by the beneficiary" beneficiaryAmongSigners)
      (traceIfFalse "The deadline hasn't passed yet" afterDeadline)
    where
      beneficiaryAmongSigners = beneficiary `elem` txInfoSignatories
      afterDeadline = PlutusV1.from (deadline + 1) `contains` txInfoValidRange

mkOracleFinalValidator WolframOracleDatum {choiceName, marloweTx, deadline} Execute (WolframScriptContext WolframTxInfo {txInfoValidRange, txInfoRedeemers} _) = 
    (&&)
      (traceIfFalse "The deadline has passed" beforeDeadline)
      -- We either traces the error message if any, or compare the embedded
      -- name with the expected one, tracing another error if they differ
      ( case eitherErrorOrRedeemerChoiceName of
          Left err -> trace err False
          Right redeemerChoiceName -> traceIfFalse "The choice names don't match" $ redeemerChoiceName == choiceName
      )
    where
      beforeDeadline = PlutusV1.to deadline `contains` txInfoValidRange
      -- This lives in the Either monad. We attempt to retrieve the name
      -- embedded in the Marlowe redeemer, while specifying relevant error
      -- messages along the way when necessary.
      eitherErrorOrRedeemerChoiceName = do
        Script.Redeemer marloweRedeemerData <-
          maybeToEither "The Marlowe input is not spent with a redeemer in the transaction"
            $ lookup (Spending marloweTx) txInfoRedeemers
        marloweRedeemer <-
          maybeToEither "The Marlowe redeemer is not a list of inputs"
            $ PlutusTx.fromBuiltinData @ExpectedRedeemerStructure marloweRedeemerData
        case marloweRedeemer of
          [MTypes.NormalInput (MTypes.IChoice (MTypes.ChoiceId redeemerChoiceName _) _)] -> return redeemerChoiceName
          [MTypes.MerkleizedInput (MTypes.IChoice (MTypes.ChoiceId redeemerChoiceName _) _) _ _] -> return redeemerChoiceName
          _ -> Left "The Marlowe redeemer is not a singleton"

{-# INLINEABLE maybeToEither #-}
maybeToEither :: err -> Maybe a -> Either err a
maybeToEither e Nothing = Left e
maybeToEither _ (Just v) = Right v

{-mkOracleFinalValidator datum redeemer ctx = 
    --traceIfFalse "Specified UTXO not in Spent Inputs" txIdInDatum &&
    --traceIfFalse "Redeemer does not match with expected structure" (redeemerMatch referenceRedeemer)
    traceIfFalse "No conditions met to claim this contract" xorConditions
    --traceIfFalse (Contexts.txInfoValidRange info) False
    where
        xorConditions :: Bool
        xorConditions = (aCondition && not bCondition) || (not aCondition && bCondition)
        
        aCondition :: Bool
        aCondition = traceBool "Redeemer Match: True" "Redeemer Match: False" (redeemerMatchQ referenceRedeemer)

        bCondition ::Bool
        bCondition = traceBool "PosixTime and Signer: True" "PosixTime and Signer: False" (posixAndSignerValidation)
        
        -- Access ctx information
        info :: PlutusV2.TxInfo
        info = PlutusV2.scriptContextTxInfo ctx
        
        -- Unpacking datum objects
        transactionID :: PlutusV2.TxId
        transactionID = PlutusV2.txOutRefId (marloweTx datum)

        inputIndex :: Integer
        inputIndex = PlutusV2.txOutRefIdx (marloweTx datum)

        internalChoiceName :: Builtins.BuiltinByteString
        internalChoiceName = choiceName datum

        limitTime :: PlutusV1.POSIXTime
        limitTime = deadline datum

        internalBeneficiary :: PlutusV2.PubKeyHash
        internalBeneficiary = beneficiary datum

        -- Check if specified UTXO is present as an Input
        -- txIdInDatum = Contexts.spendsOutput info (transactionId datum) (transactionIndex datum)
        
        -- Extract redeemer associated to UTXO stated (this also ensures the presence of UTXO as input)
        referenceRedeemer :: Maybe Redeemer
        referenceRedeemer = PlutusTx.AssocMap.lookup (Contexts.Spending (Contexts.TxOutRef transactionID inputIndex)) (txInfoRedeemers info)
        
        -- Definition to check if redeemer matches with choice
        redeemerMatchQ :: Maybe Redeemer -> Bool
        redeemerMatchQ (Just r) = choiceMatch (getRedeemer r) (internalChoiceName)
        redeemerMatchQ Nothing = False
        
        -- Definition to check if some BuiltinData matches with the defined structure of a Marlowe choice.
        choiceMatch :: BuiltinData -> Builtins.BuiltinByteString -> Bool
        choiceMatch builtInRedeemerData choiceNameFromDatum = 
            case PlutusV2.fromBuiltinData builtInRedeemerData :: Maybe ExpectedRedeemerStructure of
                Just ([marloweInput::MTypes.Input]) -> 
                    case MTypes.getInputContent marloweInput of
                        MTypes.IChoice choiceId chosenNum -> 
                            case choiceId of
                                MTypes.ChoiceId choiceName _ -> choiceName == (choiceNameFromDatum)
                                _ -> False
                        _ -> False
                Nothing -> False
        
        --POSIXTime and Signers Validation
        txValidRange :: POSIXTimeRange
        txValidRange = Contexts.txInfoValidRange info

        posixAndSignerValidation :: Bool
        posixAndSignerValidation = (contains (from (limitTime + 1)) txValidRange)  &&
                                   (Contexts.txSignedBy info internalBeneficiary) -}

{-# INLINABLE untypedWolframOracleValidator #-}
untypedWolframOracleValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
untypedWolframOracleValidator dat red ctx =
    case do
        typedDat <- PlutusTx.fromBuiltinData @WolframOracleDatum dat
        typedRed <- PlutusTx.fromBuiltinData @WolframOracleRedeemer red
        typedCtx <- PlutusTx.fromBuiltinData @WolframScriptContext ctx
        return $ mkOracleFinalValidator typedDat typedRed typedCtx of
            Nothing -> traceError "Error during deserialization"
            Just b -> check b
-----------------------------------------------
-- Export data types for Oracle
-----------------------------------------------
data OracleTypes
instance Scripts.ValidatorTypes OracleTypes where
    type instance DatumType OracleTypes = WolframOracleDatum
    type instance RedeemerType OracleTypes = WolframOracleRedeemer

-----------------------------------------------
-- Compilation prelude
-----------------------------------------------
oracleFinalContractInst :: Validator
oracleFinalContractInst = PSU.Scripts.mkValidatorScript 
    $$(PlutusTx.compile [|| untypedWolframOracleValidator ||])

--oracleFinalContractValidator :: Validator
--oracleFinalContractValidator = PSU.V2.validatorScript $ oracleFinalContractInst