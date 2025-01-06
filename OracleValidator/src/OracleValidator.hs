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

module OracleValidator where

import           GHC.Generics                         (Generic)
import           Prelude                              (Show (..), IO, print)
import qualified PlutusTx
import qualified PlutusTx.Builtins                    as Builtins
import           PlutusTx.Prelude                     hiding (Semigroup (..), unless)
import           Ledger                               hiding (singleton)
import           Ledger.Typed.Scripts                 as Scripts
import qualified Plutus.V2.Ledger.Api                 as PlutusV2
import qualified Plutus.V1.Ledger.Api                 as PlutusV1
import           Plutus.V2.Ledger.Contexts            as Contexts
import qualified Plutus.Script.Utils.Typed            as PSU
import qualified Plutus.Script.Utils.V2.Typed.Scripts as PSU.V2
import           PlutusTx.AssocMap

import qualified Types

-----------------------------------------------
-- Defining Datum data structure.
-----------------------------------------------
data UTXODatum = UTXODatum
     { transactionId :: PlutusV2.TxId,
       transactionIndex :: Integer,
       choiceGivenName :: Builtins.BuiltinByteString,
       dataTag :: Builtins.BuiltinByteString,
       deadlineLimit:: PlutusV2.POSIXTime,
       beneficiaryAfterDeadline:: PlutusV2.PubKeyHash
     } deriving Show
PlutusTx.unstableMakeIsData ''UTXODatum

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
mkOracleFinalValidator ::UTXODatum -> () -> PlutusV2.ScriptContext -> Bool
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
        transactionID = transactionId datum

        inputIndex :: Integer
        inputIndex = transactionIndex datum

        choiceName :: Builtins.BuiltinByteString
        choiceName = choiceGivenName datum

        limitTime :: PlutusV1.POSIXTime
        limitTime = deadlineLimit datum

        beneficiary :: PlutusV2.PubKeyHash
        beneficiary = beneficiaryAfterDeadline datum

        -- Check if specified UTXO is present as an Input
        -- txIdInDatum = Contexts.spendsOutput info (transactionId datum) (transactionIndex datum)
        
        -- Extract redeemer associated to UTXO stated (this also ensures the presence of UTXO as input)
        referenceRedeemer :: Maybe Redeemer
        referenceRedeemer = PlutusTx.AssocMap.lookup (Contexts.Spending (Contexts.TxOutRef transactionID inputIndex)) (txInfoRedeemers info)
        
        -- Definition to check if redeemer matches with choice
        redeemerMatchQ :: Maybe Redeemer -> Bool
        redeemerMatchQ (Just r) = choiceMatch (getRedeemer r) (choiceName)
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
                                   (Contexts.txSignedBy info beneficiary)


-----------------------------------------------
-- Export data type for Datum 
-----------------------------------------------
data OracleDatum
instance Scripts.ValidatorTypes OracleDatum where
    type instance DatumType OracleDatum = UTXODatum

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