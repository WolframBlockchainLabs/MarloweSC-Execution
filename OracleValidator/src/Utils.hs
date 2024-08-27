{-# LANGUAGE GADTs            #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Utils
    ( writeJSON
    , writeUnit
    , writeOracleValidator
    , writeOracleDatum
    ) where

import           Cardano.Api
import           Cardano.Api.Shelley   (PlutusScript (..))
import           Codec.Serialise       (serialise)
import           Data.Aeson            (encode)
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.ByteString.Short as SBS
import           PlutusTx              (Data (..))
import qualified PlutusTx
import           Plutus.V1.Ledger.Api  as V1
import qualified Plutus.V2.Ledger.Api  as Plutus
import qualified Ledger
import qualified Data.Map.Strict as Map
import           Data.Map.Strict (Map)
import qualified PlutusTx.Builtins     as Builtins

import Types

import OracleValidator

dataToScriptData :: Data -> ScriptData
dataToScriptData (Constr n xs) = ScriptDataConstructor n $ dataToScriptData <$> xs
dataToScriptData (Map xs)      = ScriptDataMap [(dataToScriptData x, dataToScriptData y) | (x, y) <- xs]
dataToScriptData (List xs)     = ScriptDataList $ dataToScriptData <$> xs
dataToScriptData (I n)         = ScriptDataNumber n
dataToScriptData (B bs)        = ScriptDataBytes bs

writeJSON :: PlutusTx.ToData a => FilePath -> a -> IO ()
writeJSON file = LBS.writeFile file . encode . scriptDataToJson ScriptDataJsonDetailedSchema . dataToScriptData . PlutusTx.toData

-- compile any validator as .plutus file
writeValidator :: FilePath -> Ledger.Validator -> IO (Either (FileError ()) ())
writeValidator file = writeFileTextEnvelope @(PlutusScript PlutusScriptV2) file Nothing . PlutusScriptSerialised . SBS.toShort . LBS.toStrict . serialise . Ledger.unValidatorScript

-- compile Oracle validator as .plutus file
writeOracleValidator :: IO (Either (FileError ()) ())
writeOracleValidator = writeValidator "scripts/oracleWithRedeemer.plutus" $ oracleValidatorContractValidator

-- Write Unit to json
writeUnit :: IO ()
writeUnit = writeJSON "scripts/unit.json" ()

-- Write Datum to deploy oracle
writeOracleDatum :: IO()
writeOracleDatum = writeJSON "scripts/oracle.datum" $ OracleValidator.UTXODatum {
    marloweContract = "03ada6af2b4644d1c59f5fbfb899869f2a4fc8eea6381f6609c9ceeb7e9c6409",
    marloweIndex = 0,
    transactionId = "ff633400eb84989f16e56e4cce7b7ee547ea595580c0e558c9b6573a834277f3",
    transactionIndex = 0,
    choiceGivenName = "oracle input",
    dataTag = "ADAUSD",
    deadlineLimit = 1715381224000,
    beneficiaryAfterDeadline = "247e0da4e1ac4b96868b1e4bf2b94adade1c58c4555409d12eb0a3b1"
}