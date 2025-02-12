{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Utils
  ( writeJSON,
    writeOracleValidator,
    writeDatum,
    writeRedeemerExecute,
    writeRedeemerReclaim,
  )
where

import Cardano.Api
import Cardano.Api.Shelley (PlutusScript (..))
import Codec.Serialise (serialise)
import Data.Aeson (encode)
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Short qualified as SBS
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Ledger qualified
-- import PlutusTx.Builtins (stringToBuiltinByteString)

import OracleValidator
import PlutusLedgerApi.V1 as V1
import PlutusLedgerApi.V2 qualified as V2
import PlutusTx (Data (..))
import PlutusTx qualified
import PlutusTx.Builtins qualified as Builtins

dataToScriptData :: Data -> ScriptData
dataToScriptData (Constr n xs) = ScriptDataConstructor n $ dataToScriptData <$> xs
dataToScriptData (Map xs) = ScriptDataMap [(dataToScriptData x, dataToScriptData y) | (x, y) <- xs]
dataToScriptData (List xs) = ScriptDataList $ dataToScriptData <$> xs
dataToScriptData (I n) = ScriptDataNumber n
dataToScriptData (B bs) = ScriptDataBytes bs

writeJSON :: (PlutusTx.ToData a) => FilePath -> a -> IO ()
writeJSON file = LBS.writeFile file . encode . scriptDataToJson ScriptDataJsonDetailedSchema . unsafeHashableScriptData . dataToScriptData . PlutusTx.toData

-- compile any validator as .plutus file
writeValidator :: FilePath -> Ledger.Validator -> IO (Either (FileError ()) ())
writeValidator file = writeFileTextEnvelope @(PlutusScript PlutusScriptV2) (File file) Nothing . PlutusScriptSerialised . SBS.toShort . LBS.toStrict . serialise . Ledger.unValidatorScript

-- compile Oracle validator as .plutus file
writeOracleValidator :: IO (Either (FileError ()) ())
writeOracleValidator = writeValidator "scripts/oracleValidator.plutus" wolframOracleCompiledValidator

-- write validator Datum
writeDatum :: IO ()
writeDatum =
  writeJSON "scripts/anotherOracleDatum.datum" $
    WolframOracleDatum
      { marloweTx = V2.TxOutRef "03ada6af2b4644d1c59f5fbfb899869f2a4fc8eea6381f6609c9ceeb7e9c6409" 0,
        choiceName = "oracle input",
        dataTag = "ADA/USD",
        deadline = 1724459986000,
        beneficiary = "f1ca04a98e903273b9f3853b9888a1dc62a704ef0801f04b3e71538b"
      }

writeRedeemerExecute :: IO ()
writeRedeemerExecute = writeJSON "scripts/oracleRedeemerExecute.redeemer" Execute

writeRedeemerReclaim :: IO ()
writeRedeemerReclaim = writeJSON "scripts/oracleRedeemerReclaim.redeemer" Reclaim
