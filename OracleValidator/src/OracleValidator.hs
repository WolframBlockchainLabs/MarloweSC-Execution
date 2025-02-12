{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module OracleValidator where

import GHC.Generics (Generic)
import Ledger (Validator) -- hiding (singleton, txInfoSignatores, txInfoValidRange)
import Ledger.Typed.Scripts as Scripts
import MinimalTypes qualified as MTypes
import Plutus.Script.Utils.Scripts qualified as PSU.Scripts
import Plutus.Script.Utils.Typed qualified as PSU
import Plutus.Script.Utils.V2.Typed.Scripts qualified as PSU.V2
import PlutusLedgerApi.V1 qualified as PlutusV1
import PlutusLedgerApi.V1.Interval (contains)
import PlutusLedgerApi.V1.Scripts qualified as Script
import PlutusLedgerApi.V2 qualified as PlutusV2
import PlutusLedgerApi.V2.Contexts as Contexts (ScriptPurpose (..))
import PlutusTx qualified
import PlutusTx.AssocMap
import PlutusTx.Builtins qualified as Builtins
import PlutusTx.Prelude hiding (Semigroup (..), unless)
import Prelude qualified as P (Eq (..), IO, Show (..), print)

-----------------------------------------------
-- Defining Datum.
-----------------------------------------------
data WolframOracleDatum = WolframOracleDatum
  { marloweTx :: PlutusV2.TxOutRef,
    choiceName :: Builtins.BuiltinByteString,
    dataTag :: Builtins.BuiltinByteString,
    deadline :: PlutusV2.POSIXTime,
    beneficiary :: PlutusV2.PubKeyHash
  }
  deriving (P.Show, P.Eq)

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
  { txInfoInputs :: BuiltinData,
    txInfoReferenceInputs :: BuiltinData,
    txInfoOutputs :: BuiltinData,
    txInfoFee :: BuiltinData,
    txInfoMint :: BuiltinData,
    txInfoTxCerts :: BuiltinData,
    txInfoWdrl :: BuiltinData,
    txInfoValidRange :: PlutusV1.POSIXTimeRange,
    txInfoSignatories :: [PlutusV2.PubKeyHash],
    txInfoRedeemers :: Map Contexts.ScriptPurpose Script.Redeemer,
    txInfoData :: BuiltinData,
    txInfoId :: BuiltinData,
    txInfoVotes :: BuiltinData,
    txInfoProposalProcedures :: BuiltinData,
    txInfoCurrentTreasuryAmount :: BuiltinData,
    txInfoTreasuryDonation :: BuiltinData
  }
  deriving (P.Eq)

instance Eq WolframTxInfo where
  (==) = (==)

PlutusTx.unstableMakeIsData ''WolframTxInfo

data WolframScriptContext = WolframScriptContext
  { scriptContextTxInfo :: WolframTxInfo,
    scriptContextPurpose :: BuiltinData
  }
  deriving (P.Eq)

instance Eq WolframScriptContext where
  (==) = (==)

PlutusTx.unstableMakeIsData ''WolframScriptContext

-----------------------------------------------
-- Defining Marlowe expected redeemer
-----------------------------------------------
type ExpectedRedeemerStructure = [MTypes.Input]

--------------------------------------------------------
--
-- Wolfram Oracle Validator
--
--------------------------------------------------------
-----------------------------------------------
-- Authorized Wolfram wallet to use Execute.
-----------------------------------------------
{-# INLINEABLE wolframStaticWallet #-}
wolframStaticWallet :: PlutusV2.PubKeyHash
wolframStaticWallet = PlutusV2.PubKeyHash ("f1ca04a98e903273b9f3853b9888a1dc62a704ef0801f04b3e71538b" :: BuiltinByteString)

-----------------------------------------------
-- Defining on-chain validator for oracle.
-----------------------------------------------
{-# INLINEABLE wolframOracleValidator #-}
wolframOracleValidator :: WolframOracleDatum -> WolframOracleRedeemer -> WolframScriptContext -> Bool

-- Reclaim endpoint
wolframOracleValidator WolframOracleDatum {beneficiary, deadline} Reclaim (WolframScriptContext WolframTxInfo {txInfoSignatories, txInfoValidRange} _) =
  (&&)
    (traceIfFalse "The transaction wasn't signed by the beneficiary" beneficiaryAmongSigners)
    (traceIfFalse "The deadline hasn't passed yet" afterDeadline)
  where
    beneficiaryAmongSigners = beneficiary `elem` txInfoSignatories
    afterDeadline = PlutusV1.from (deadline + 1) `contains` txInfoValidRange

-- Execute endpoint
wolframOracleValidator WolframOracleDatum {choiceName, marloweTx, deadline} Execute (WolframScriptContext WolframTxInfo {txInfoValidRange, txInfoRedeemers, txInfoSignatories} _) =
  traceIfFalse "The deadline has passed" beforeDeadline
    && ( case eitherErrorOrRedeemerChoiceName of
           Left err -> trace err False
           Right redeemerChoiceName -> traceIfFalse "The choice names don't match" $ redeemerChoiceName == choiceName
       )
    && traceIfFalse "The transaction wasn't signed by the authorized Wolfral wallet" signedByWolframWallet
  where
    beforeDeadline = PlutusV1.to deadline `contains` txInfoValidRange
    signedByWolframWallet = wolframStaticWallet `elem` txInfoSignatories
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

-----------------------------------------------
-- Auxiliary functions taken from Tweag Audit
-----------------------------------------------
{-# INLINEABLE maybeToEither #-}
maybeToEither :: err -> Maybe a -> Either err a
maybeToEither e Nothing = Left e
maybeToEither _ (Just v) = Right v

-----------------------------------------------
-- Custom typed-to-untyped wrapper
-----------------------------------------------
{-# INLINEABLE untypedWolframOracleValidator #-}
untypedWolframOracleValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
untypedWolframOracleValidator dat red ctx =
  case do
    typedDat <- PlutusTx.fromBuiltinData @WolframOracleDatum dat
    typedRed <- PlutusTx.fromBuiltinData @WolframOracleRedeemer red
    typedCtx <- PlutusTx.fromBuiltinData @WolframScriptContext ctx
    return $ wolframOracleValidator typedDat typedRed typedCtx of
    Nothing -> traceError "Error during deserialization"
    Just b -> check b

-----------------------------------------------
-- Export data types for Oracle
-----------------------------------------------
data OracleTypes

instance Scripts.ValidatorTypes OracleTypes where
  type DatumType OracleTypes = WolframOracleDatum
  type RedeemerType OracleTypes = WolframOracleRedeemer

-----------------------------------------------
-- Compilation prelude
-----------------------------------------------
wolframOracleCompiledValidator :: Validator
wolframOracleCompiledValidator =
  PSU.Scripts.mkValidatorScript
    $$(PlutusTx.compile [||untypedWolframOracleValidator||])