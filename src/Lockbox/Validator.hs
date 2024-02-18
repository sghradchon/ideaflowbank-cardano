{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Lockbox.Validator (validator) where

import           Plutus.Script.Utils.Typed                       (mkUntypedValidator)
import           Plutus.Script.Utils.V1.Typed.Scripts.Validators (DatumType,
                                                                  RedeemerType)
import           Plutus.Script.Utils.V2.Typed.Scripts            (TypedValidator,
                                                                  ValidatorTypes,
                                                                  mkTypedValidatorParam,
                                                                  validatorScript)
import           Plutus.V1.Ledger.Interval                       (contains)
import           Plutus.V2.Ledger.Api
import           PlutusTx
import           PlutusTx.Prelude                                hiding
                                                                 (Semigroup (..),
                                                                  unless)

import           Lockbox.Types


{-# INLINEABLE mkValidator #-}
mkValidator :: LockboxParams -> LockboxDatum -> LockboxRedeemer -> ScriptContext -> Bool
mkValidator LockboxParams{..} datum redeemer ctx =
    let
        info :: TxInfo
        info = scriptContextTxInfo ctx

    in case (redeemer, datum) of

        (FirstClaim redeemerNumber, LockboxDatum datumNumber) ->
                traceIfFalse "First Deadline Has NOT Been Reached"
                    (from firstDeadline `contains` txInfoValidRange info )
            &&  traceIfFalse "Numbers Mismatched"
                    (       number1 == redeemerNumber
                        &&  number1 == datumNumber
                    )

        (SecondClaim redeemerNumber, LockboxDatum datumNumber) ->
                traceIfFalse "Second Deadline Has NOT Been Reached"
                    (from secondDeadline `contains` txInfoValidRange info)
            &&  traceIfFalse "Third Deadline Has Been Reached"
                    (to thirdDeadline `contains` txInfoValidRange info)
            &&  traceIfFalse "Numbers Mismatched"
                    (       number2 == redeemerNumber
                        &&  number2 == datumNumber
                    )

        (ThirdClaim redeemerNumber, LockboxDatum datumNumber) ->
                traceIfFalse "Third Deadline Has NOT Been Reached"
                    (from thirdDeadline `contains` txInfoValidRange info)
            &&  traceIfFalse "Numbers Mismatched"
                    (       number3 == redeemerNumber
                        &&  number3 == datumNumber
                    )

data LockboxTypes

instance ValidatorTypes LockboxTypes where
  type DatumType LockboxTypes = LockboxDatum
  type RedeemerType LockboxTypes = LockboxRedeemer

typedValidator :: LockboxParams -> TypedValidator LockboxTypes
typedValidator = go
  where
    go =
      mkTypedValidatorParam @LockboxTypes
        $$(PlutusTx.compile [||mkValidator||])
        $$(PlutusTx.compile [||wrap||])
    wrap = mkUntypedValidator

validator :: LockboxParams -> Validator
validator = validatorScript . typedValidator
