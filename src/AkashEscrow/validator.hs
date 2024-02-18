module AkashEscrow.Validator where

data AkashEscrowParams =
    AkashEscrowParams
    {   currencySymbolOfLandMinter :: CurrencySymbol
    ,   akashPKH                   :: PubKeyHash
    }

PlutusTx.makeLift ''AkashEscrowParams

data AkashEscrowDatum =
    AkashEscrowDatum
        {   landID        :: TokenName
        ,   price         :: Integer
        ,   sellerAddress :: Address
        }

instance Eq AkashEscrowDatum where
    {-# INLINABLE (==) #-}
    AkashEscrowDatum a b c == AkashEscrowDatum a' b' c'
        | a == a' && b == b' && c == c' = True
    _ == _ = False

PlutusTx.makeIsDataIndexed ''AkashEscrowDatum   [ ( 'AkashEscrowDatum, 1)]

data AkashEscrowAction =
    BuyLand
    | SellLand
    | RetrieveLand
    | UpdateLand
    | AkashAction


mkValidator :: AkashEscrowParams -> AkashEscrowDatum -> AkashEscrowAction -> ScriptContext -> Bool
mkValidator params datum redeemer ctx =
    case redeemer of
        BuyLand ->
            traceIfFalse "Seller Must Paid"
                (isSellerPaid sellerAddress price)

        SellLand ->
            True

        UpdateLand ->
            False

        AkashAction ->
            True

        _ -> False

    where

        isSellerPaid :: Address -> Integer -> Bool
        isSellerPaid addr amt
            | Just i <- find (\i -> valueOf (txOutValue (txInfoResolved i)) adaCurrencySymbol adaTokenName == amt) txInfoInputs = True
            | otherwise = False
