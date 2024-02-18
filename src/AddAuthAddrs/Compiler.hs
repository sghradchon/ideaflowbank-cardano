{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module AddAuthAddrs.Compiler (writeAddAuthAddrsScript, writeTypedAddAuthAddrsScript) where

import Cardano.Api
import Codec.Serialise (serialise)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS
import qualified Plutus.V2.Ledger.Api
import Cardano.Api.Shelley (PlutusScript (..))
import PlutusTx.Prelude
import Prelude (FilePath, IO)

import AddAuthAddrs.Validator as AddAuthAddrs


writeValidator :: FilePath -> Plutus.V2.Ledger.Api.Validator -> IO (Either (FileError ()) ())
writeValidator file = writeFileTextEnvelope @(PlutusScript PlutusScriptV2) file Nothing . PlutusScriptSerialised . SBS.toShort . LBS.toStrict . serialise . Plutus.V2.Ledger.Api.unValidatorScript

writeAddAuthAddrsScript :: IO (Either (FileError ()) ())
writeAddAuthAddrsScript = writeValidator "output/add-auth-addrs.plutus" AddAuthAddrs.validator