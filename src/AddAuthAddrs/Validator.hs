{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- PlutusV2
module AddAuthAddrs.Validator (validator) where

import Plutus.V1.Ledger.Value
import Plutus.V2.Ledger.Api
import Plutus.V2.Ledger.Contexts
import Plutus.V1.Ledger.Interval
import PlutusTx
import PlutusTx.Prelude hiding (Semigroup (..), unless)

-- コントラクトの所有者を指定
ownerPubKeyHash :: PubKeyHash
ownerPubKeyHash = PubKeyHash $ C.pack "addr_test1qqfafptpds9hqtxhndugcz9v9z9f6j0w7fpp7hygpxraajw0f0uym3yqlw3u4ks7luxatckz3yvtthpsr8cj36v0tqts97hedt"

-- 権限付与されたアドレスのリストを保持するデータ型
data AuthorizedAddresses = AuthorizedAddresses { getAddresses :: [PubKeyHash] }
PlutusTx.unstableMakeIsData ''AuthorizedAddresses

{-# INLINEABLE mkValidator #-}
mkValidator :: AuthorizedAddresses -> () -> ScriptContext -> Bool
mkValidator _ _ ctx = traceIfFalse "Unauthorized access" $ txSignedBy (scriptContextTxInfo ctx) ownerPubKeyHash

typedValidator :: TypedValidator AuthorizedAddresses
typedValidator = mkTypedValidator @AuthorizedAddresses
    $$(PlutusTx.compile [|| mkValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = wrapValidator @AuthorizedAddresses @()

validator :: Validator
validator = validatorScript typedValidator
