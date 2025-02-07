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

module OracleValidator where

import           GHC.Generics                         (Generic)
import qualified Prelude                              as P (Show (..), Eq(..), IO, print)
import qualified PlutusTx
import qualified PlutusTx.Builtins                    as Builtins
import           PlutusTx.Prelude                     hiding (Semigroup (..), unless)
import           Ledger                               hiding (singleton)
import           Ledger.Typed.Scripts                 as Scripts
import qualified PlutusLedgerApi.V1                   as PlutusV1
import qualified PlutusLedgerApi.V2                   as PlutusV2
import           PlutusLedgerApi.V2.Contexts          as Contexts
import qualified Plutus.Script.Utils.Typed            as PSU
import qualified Plutus.Script.Utils.V2.Typed.Scripts as PSU.V2
import           PlutusTx.AssocMap

import qualified Types

-----------------------------------------------
-- Defining Datum data structure.
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

type ExpectedRedeemerStructure = [Types.Input]

--------------------------------------------------------
--
-- Oracle validator With datum.
--
--------------------------------------------------------
-----------------------------------------------
-- Defining on-chain validator for oracle.
-----------------------------------------------
{-# INLINABLE mkOracleFinalValidator #-}
mkOracleFinalValidator ::WolframOracleDatum -> () -> PlutusV2.ScriptContext -> Bool
mkOracleFinalValidator datum _ ctx = 
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
                Just ([marloweInput::Types.Input]) -> 
                    case Types.getInputContent marloweInput of
                        Types.IChoice choiceId chosenNum -> 
                            case choiceId of
                                Types.ChoiceId choiceName _ -> choiceName == (choiceNameFromDatum)
                                _ -> False
                        _ -> False
                Nothing -> False
        
        --POSIXTime and Signers Validation
        txValidRange :: POSIXTimeRange
        txValidRange = Contexts.txInfoValidRange info

        posixAndSignerValidation :: Bool
        posixAndSignerValidation = (contains (from (limitTime + 1)) txValidRange)  &&
                                   (Contexts.txSignedBy info internalBeneficiary)


-----------------------------------------------
-- Export data type for Datum 
-----------------------------------------------
data OracleDatum
instance Scripts.ValidatorTypes OracleDatum where
    type instance DatumType OracleDatum = WolframOracleDatum

-----------------------------------------------
-- Compilation prelude
-----------------------------------------------
oracleFinalContractInst :: PSU.TypedValidator OracleDatum
oracleFinalContractInst = PSU.V2.mkTypedValidator @OracleDatum
    $$(PlutusTx.compile [|| mkOracleFinalValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = PSU.mkUntypedValidator

oracleFinalContractValidator :: Validator
oracleFinalContractValidator = PSU.V2.validatorScript $ oracleFinalContractInst