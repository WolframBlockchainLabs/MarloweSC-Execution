{-# LANGUAGE GADTs            #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Utils
    ( writeJSON
    , writeUnit
    , writeOracleValidator
    , writeAlwaysSucceedsValidator
    , writeMarloweRedeemer
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
--import PlutusTx.Builtins (stringToBuiltinByteString)

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
writeOracleValidator = writeValidator "scripts/oracleValidator.plutus" $ oracleFinalContractValidator

--write validator Datum
writeDatum :: IO ()
writeDatum = writeJSON "scripts/oracleDatum.datum" $ OracleValidator.UTXODatum {
    transactionId = "03ada6af2b4644d1c59f5fbfb899869f2a4fc8eea6381f6609c9ceeb7e9c6409" ,
    transactionIndex = 0 ,
    choiceGivenName = "oracle input" ,
    dataTag = "ADA/USD",
    deadlineLimit = 1724459986000,
    beneficiaryAfterDeadline = "f1ca04a98e903273b9f3853b9888a1dc62a704ef0801f04b3e71538b"
}

-- Write Unit to json
writeUnit :: IO ()
writeUnit = writeJSON "scripts/unit.json" ()

pubkeyhash::Plutus.PubKeyHash
pubkeyhash = "f1ca04a98e903273b9f3853b9888a1dc62a704ef0801f04b3e71538b"

writeMarloweRedeemer :: IO ()
writeMarloweRedeemer = 
    do
        let pkc = V1.PubKeyCredential pubkeyhash
        let addr = Plutus.Address pkc Nothing
        let party = Types.Address Types.testnet addr
        let name = "oracle input"
        let choiceid = Types.ChoiceId name party
        let num = 10000
        let content = Types.IChoice choiceid num
        let input = Types.NormalInput content
        let redeemer = [input]

        writeJSON "scripts/oracleInput.redeemer" $ redeemer
