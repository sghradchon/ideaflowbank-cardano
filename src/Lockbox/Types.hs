{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

module Lockbox.Types
    (   LockboxParams   (..)
    ,   LockboxDatum    (..)
    ,   LockboxRedeemer (..)
    ) where

import           Plutus.V2.Ledger.Api
import qualified PlutusTx
import           PlutusTx.Prelude     hiding (Semigroup (..), unless)

data LockboxParams = LockboxParams
    {   firstDeadline  :: !POSIXTime
    ,   secondDeadline :: !POSIXTime
    ,   thirdDeadline  :: !POSIXTime
    ,   number1        :: !Integer
    ,   number2        :: !Integer
    ,   number3        :: !Integer
    }

PlutusTx.makeLift ''LockboxParams


newtype LockboxDatum = LockboxDatum Integer

instance Eq LockboxDatum where
    {-# INLINABLE (==) #-}
    LockboxDatum a == LockboxDatum a' | a == a' = True
    _ == _ = False

PlutusTx.makeIsDataIndexed ''LockboxDatum   [ ( 'LockboxDatum, 1 )]

data LockboxRedeemer
    =   FirstClaim Integer
    |   SecondClaim Integer
    |   ThirdClaim Integer

instance Eq LockboxRedeemer where
    {-# INLINABLE (==) #-}
    FirstClaim a    == FirstClaim a'    | a == a' = True
    SecondClaim a   == SecondClaim a'   | a == a' = True
    ThirdClaim a    == ThirdClaim a'    | a == a' = True
    _ == _ = False

PlutusTx.makeIsDataIndexed ''LockboxRedeemer    [ ( 'FirstClaim,    1 )
                                                , ( 'SecondClaim,   2 )
                                                , ( 'ThirdClaim,    3 )
                                                ]
