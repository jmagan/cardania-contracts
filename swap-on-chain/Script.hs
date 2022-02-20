{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}

module Script
  ( burnSerialised,
    burnSBS,
  )
where

import qualified Cardano.Api
import qualified Cardano.Api.Shelley
import qualified Codec.Serialise
import qualified Data.Aeson
import qualified Data.ByteString
import qualified Data.ByteString.Lazy
import qualified Data.ByteString.Short
import qualified Ledger.Typed.Scripts
import qualified Plutus.V1.Ledger.Ada
import qualified Plutus.V1.Ledger.Address
import qualified Plutus.V1.Ledger.Api
import qualified Plutus.V1.Ledger.Contexts
import qualified Plutus.V1.Ledger.Scripts
import qualified PlutusTx
import qualified PlutusTx.Lift.Class
import qualified PlutusTx.Prelude
import qualified Prelude

data Burn

instance Ledger.Typed.Scripts.ValidatorTypes Burn where
  type RedeemerType Burn = PlutusTx.Prelude.Integer
  type DatumType Burn = PlutusTx.Prelude.Integer

{-# INLINEABLE mkBurnValidator #-}
mkBurnValidator :: PlutusTx.Prelude.Integer -> PlutusTx.Prelude.Integer -> Plutus.V1.Ledger.Contexts.ScriptContext -> PlutusTx.Prelude.Bool
mkBurnValidator _ _ context = PlutusTx.Prelude.False

typedValidator :: Ledger.Typed.Scripts.TypedValidator Burn
typedValidator =
  Ledger.Typed.Scripts.mkTypedValidator @Burn
    $$(PlutusTx.compile [||mkBurnValidator||])
    $$(PlutusTx.compile [||Ledger.Typed.Scripts.wrapValidator @PlutusTx.Prelude.Integer @PlutusTx.Prelude.Integer||])

validator :: Plutus.V1.Ledger.Scripts.Validator
validator = Ledger.Typed.Scripts.validatorScript typedValidator

burnScript :: Plutus.V1.Ledger.Scripts.Script
burnScript = Plutus.V1.Ledger.Scripts.unValidatorScript validator

burnSBS :: Data.ByteString.Short.ShortByteString
burnSBS = Data.ByteString.Short.toShort PlutusTx.Prelude.. Data.ByteString.Lazy.toStrict PlutusTx.Prelude.$ Codec.Serialise.serialise burnScript

burnSerialised :: Cardano.Api.PlutusScript Cardano.Api.PlutusScriptV1
burnSerialised = Cardano.Api.Shelley.PlutusScriptSerialised burnSBS
