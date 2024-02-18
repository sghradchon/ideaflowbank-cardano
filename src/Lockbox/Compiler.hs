{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Lockbox.Compiler
    (   createOutputFolder
    ,   writeLockboxScript
    ,   writeDatum1
    ,   writeDatum2
    ,   writeDatum3
    ,   writeFirstClaimRedeemer
    ,   writeSecondClaimRedeemer
    ,   writeThirdClaimRedeemer
    ) where

import           Cardano.Api
import           Cardano.Api.Shelley   (PlutusScript (..), fromPlutusData)
import           Codec.Serialise       (serialise)
import           Data.Aeson            (encode)
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.ByteString.Short as SBS
import           Lockbox.Types
import           Lockbox.Validator
import qualified Plutus.V2.Ledger.Api
import           PlutusTx              (ToData, toData)
import           PlutusTx.Prelude
import           Prelude               (FilePath, IO)
import           System.Directory      (createDirectoryIfMissing)

createOutputFolder :: IO ()
createOutputFolder = createDirectoryIfMissing True "output"

num1 :: Integer
num1 = 1

num2 :: Integer
num2 = 2

num3 :: Integer
num3 = 3

lockboxParams :: LockboxParams
lockboxParams = LockboxParams
    {   firstDeadline  = 999999999
    ,   secondDeadline = 999999999
    ,   thirdDeadline  = 999999999
    ,   number1        = num1
    ,   number2        = num2
    ,   number3        = num3
    }

lockboxDatum1 :: LockboxDatum
lockboxDatum1 = LockboxDatum num1

lockboxDatum2 :: LockboxDatum
lockboxDatum2 = LockboxDatum num2

lockboxDatum3 :: LockboxDatum
lockboxDatum3 = LockboxDatum num3

firstClaimRedeemer :: LockboxRedeemer
firstClaimRedeemer = FirstClaim num1

secondClaimRedeemer :: LockboxRedeemer
secondClaimRedeemer = SecondClaim num2

thirdClaimRedeemer :: LockboxRedeemer
thirdClaimRedeemer = ThirdClaim num3

writeValidator :: FilePath -> Plutus.V2.Ledger.Api.Validator -> IO (Either (FileError ()) ())
writeValidator file = writeFileTextEnvelope @(PlutusScript PlutusScriptV2) file Nothing . PlutusScriptSerialised . SBS.toShort . LBS.toStrict . serialise . Plutus.V2.Ledger.Api.unValidatorScript

writeLockboxScript :: IO (Either (FileError ()) ())
writeLockboxScript = writeValidator "output/lockbox.plutus" $ validator lockboxParams

writeJSON :: ToData a => FilePath -> a -> IO ()
writeJSON file = LBS.writeFile file . encode . scriptDataToJson ScriptDataJsonDetailedSchema . fromPlutusData . toData

writeDatum1 :: IO ()
writeDatum1 = writeJSON "output/lockboxDatum1.json" lockboxDatum1

writeDatum2 :: IO ()
writeDatum2 = writeJSON "output/lockboxDatum2.json" lockboxDatum2

writeDatum3 :: IO ()
writeDatum3 = writeJSON "output/lockboxDatum3.json" lockboxDatum3

writeFirstClaimRedeemer :: IO ()
writeFirstClaimRedeemer = writeJSON "output/firstClaimRedeemer.json" firstClaimRedeemer

writeSecondClaimRedeemer :: IO ()
writeSecondClaimRedeemer = writeJSON "output/secondClaimRedeemer.json" secondClaimRedeemer

writeThirdClaimRedeemer :: IO ()
writeThirdClaimRedeemer = writeJSON "output/thirdClaimRedeemer.json" thirdClaimRedeemer
