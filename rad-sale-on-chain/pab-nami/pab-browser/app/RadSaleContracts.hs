{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module RadSaleContracts
  ( RadSaleContracts (..),
  )
where

import Data.Aeson 
import Data.OpenApi 
import Data.String qualified
import Data.Void qualified
import GHC.Generics 
import Language.PureScript.Bridge qualified
import Ledger qualified
import Ledger.Constraints qualified
import Playground.Types qualified
import Plutus.Contract 
import Plutus.PAB.Effects.Contract.Builtin qualified
import Plutus.PAB.Run.PSGenerator qualified
import Plutus.V1.Ledger.Api qualified
import PlutusTx.Builtins.Class qualified
import Prettyprinter qualified
import Schema qualified
import Script 
import Prelude
import PlutusTx.Prelude qualified
import Data.Text  (Text)
import Ledger
import Ledger.Ada qualified
import Ledger.Address as Address
import Ledger.Constraints.TxConstraints qualified
import Ledger.Constraints
import Data.Void
import Plutus.V1.Ledger.Value as Value
import Text.Printf qualified
import Data.Functor qualified
import Plutus.Contract.Typed.Tx qualified
import Data.Monoid qualified
import PlutusTx qualified
import Plutus.V1.Ledger.Api qualified
import Plutus.V1.Ledger.Scripts qualified
import Data.Map qualified

data RadSaleContracts = RadSaleContracts
  deriving (Prelude.Eq, Prelude.Ord, Prelude.Show, GHC.Generics.Generic)
  deriving anyclass (Data.Aeson.FromJSON, Data.Aeson.ToJSON, Data.OpenApi.ToSchema)

instance Prettyprinter.Pretty RadSaleContracts where
  pretty = Prettyprinter.viaShow

instance Plutus.PAB.Run.PSGenerator.HasPSTypes RadSaleContracts where
  psTypes =
    [ Language.PureScript.Bridge.order
        Prelude.. Language.PureScript.Bridge.equal
        Prelude.. Language.PureScript.Bridge.genericShow
        Prelude.. Language.PureScript.Bridge.argonaut
        Prelude.$ Language.PureScript.Bridge.mkSumType @RadSaleContracts
    ]

tokenSaleParam :: Script.TokenSaleParam
tokenSaleParam =
  Script.TokenSaleParam
    { Script.tokenCost = 100,
      Script.currencySymbol = "DemoCurrencySymbol",
      Script.tokenName = "50505050",
      Script.sellerPubKeyHash = "DemoPubKeyHash"
    }

instance Plutus.PAB.Effects.Contract.Builtin.HasDefinitions RadSaleContracts where
  getDefinitions =
    [ RadSaleContracts
    ]
  getContract = getRadSaleContract
  getSchema = getRadSaleContractSchema

getRadSaleContractSchema :: RadSaleContracts -> [Playground.Types.FunctionSchema Schema.FormSchema]
getRadSaleContractSchema contract =
  case contract of
    RadSaleContracts -> Plutus.PAB.Effects.Contract.Builtin.endpointsToSchemas @BrowserSaleSchema
    -- Buy param -> Plutus.PAB.Effects.Contract.Builtin.endpointsToSchemas @Script.SaleSchema
    -- Close param -> Plutus.PAB.Effects.Contract.Builtin.endpointsToSchemas @Script.SaleSchema

getRadSaleContract :: RadSaleContracts -> Plutus.PAB.Effects.Contract.Builtin.SomeBuiltin
getRadSaleContract contract =
  case contract of
    RadSaleContracts ->  Plutus.PAB.Effects.Contract.Builtin.SomeBuiltin pabEndPoints
    -- Buy paramPAB ->
    --   Plutus.PAB.Effects.Contract.Builtin.SomeBuiltin Prelude.$ Script.buy paramPAB
    -- Close paramPAB ->
    --   Plutus.PAB.Effects.Contract.Builtin.SomeBuiltin Prelude.$ Script.close paramPAB

type BrowserSaleSchema = Endpoint "start" TokenSaleParam 
                          .\/ Endpoint "PayToWallet" PayToWalletParams
                          .\/ Endpoint "buy" TokenSaleParam
    --Plutus.Contract..\/ Plutus.Contract.Endpoint "buy" TokenSaleParam
    --Plutus.Contract..\/ Plutus.Contract.Endpoint "close" TokenSaleParam

pabEndPoints :: Plutus.Contract.Promise () BrowserSaleSchema Text ()
pabEndPoints = Plutus.Contract.endpoint @"start" start''
                `select` Plutus.Contract.endpoint @"PayToWallet" payToWallet
                `select` Plutus.Contract.endpoint @"buy" buy''

start'' ::
  TokenSaleParam ->
  Plutus.Contract.Contract () BrowserSaleSchema Data.Text.Text ()
start'' tokenSaleParam = do
  let v =
        Plutus.V1.Ledger.Api.singleton
          (Script.currencySymbol tokenSaleParam)
          (Script.tokenName tokenSaleParam)
          1
          PlutusTx.Prelude.<> Ledger.Ada.lovelaceValueOf 2000000

  let tx = Ledger.Constraints.TxConstraints.mustPayToTheScript () v
  ledgerTx <-
    Plutus.Contract.mkTxConstraints
      (typedValidatorLookups $ typedValidator tokenSaleParam)
      tx
  yieldUnbalancedTx PlutusTx.Prelude.$ Ledger.Constraints.adjustUnbalancedTx ledgerTx

data PayToWalletParams =
    PayToWalletParams
        { amount :: Value.Value
        , pkh    :: PaymentPubKeyHash
        , skh    :: StakePubKeyHash
        }
        deriving (Eq, Show, GHC.Generics.Generic, Schema.ToSchema)
        deriving anyclass (Data.Aeson.FromJSON, Data.Aeson.ToJSON, Data.OpenApi.ToSchema)


payToWallet :: PayToWalletParams -> Contract () BrowserSaleSchema Text ()
payToWallet PayToWalletParams{amount, pkh, skh} = do
    utx <- mkTxConstraints @Void mempty (mustPayToPubKeyAddress pkh skh amount)
    logInfo @Prelude.String $ show utx
    yieldUnbalancedTx $ adjustUnbalancedTx utx


buy'' ::
  TokenSaleParam ->
  Plutus.Contract.Contract () BrowserSaleSchema Data.Text.Text ()
buy'' tokenSaleParam = do
  Plutus.Contract.logInfo @Prelude.String PlutusTx.Prelude.$
    Text.Printf.printf "started buy for token"
  --pkh <- Plutus.Contract.ownPaymentPubKeyHash
  --Plutus.Contract.logInfo @Prelude.String PlutusTx.Prelude.$
  --  Text.Printf.printf "pkh %s" (Prelude.show pkh)
  Plutus.Contract.logInfo @Prelude.String PlutusTx.Prelude.$
    Text.Printf.printf "Getting address"

  let address = Script.scrAddress tokenSaleParam

  Plutus.Contract.logInfo @Prelude.String PlutusTx.Prelude.$
    Text.Printf.printf $ show address

  Plutus.Contract.logInfo @Prelude.String PlutusTx.Prelude.$
    Text.Printf.printf "searching utxos"
  scriptUtxos <-
    -- Data.Map.filter isSuitable
    --   PlutusTx.Prelude.<$>
    Plutus.Contract.utxosAt address
  Plutus.Contract.logInfo @Prelude.String PlutusTx.Prelude.$
    Text.Printf.printf "scriptUtxos downloaded" 
  let utxosList = Data.Map.toList scriptUtxos
      utxoOref = PlutusTx.Prelude.fst (PlutusTx.Prelude.head utxosList)
      remainingUtxosList = PlutusTx.Prelude.tail utxosList
      totalValue =
        PlutusTx.Prelude.foldl
          ( \w (oref, o) ->
              w
                PlutusTx.Prelude.<> Ledger._ciTxOutValue o
          )
          PlutusTx.Prelude.mempty
          remainingUtxosList
      totalValueOfToken = Value.valueOf totalValue (Script.currencySymbol tokenSaleParam) (Script.tokenName tokenSaleParam)
      valueBackToScript =
        Plutus.V1.Ledger.Api.singleton
          (Script.currencySymbol tokenSaleParam)
          (Script.tokenName tokenSaleParam)
          (totalValueOfToken PlutusTx.Prelude.- 1)
      redeemer =
        Plutus.V1.Ledger.Scripts.Redeemer PlutusTx.Prelude.$
          PlutusTx.toBuiltinData Script.Buy
      lookups =
        Data.Monoid.mconcat
          [ Ledger.Constraints.typedValidatorLookups (typedValidator tokenSaleParam),
            Ledger.Constraints.unspentOutputs scriptUtxos,
            Ledger.Constraints.otherData (Plutus.V1.Ledger.Api.Datum (Plutus.V1.Ledger.Api.toBuiltinData ()))
          ]

      tx =
        PlutusTx.Prelude.mconcat
          [ Plutus.Contract.Typed.Tx.collectFromScript scriptUtxos Script.Buy,
            -- Ledger.Constraints.TxConstraints.mustBeSignedBy pkh,
            Ledger.Constraints.TxConstraints.mustPayToTheScript
              ()
              (Ledger.Ada.lovelaceValueOf minLovelace PlutusTx.Prelude.<> (totalValue PlutusTx.Prelude.<> valueBackToScript)),
            Ledger.Constraints.TxConstraints.mustPayToPubKey
              (PaymentPubKeyHash (sellerPubKeyHash tokenSaleParam))
              (Ledger.Ada.lovelaceValueOf (tokenCost tokenSaleParam))
            --Ledger.Constraints.TxConstraints.mustPayToPubKey
            --  pkh
            --  v
          ]

  Plutus.Contract.logInfo @Prelude.String PlutusTx.Prelude.$
    Text.Printf.printf "lookups %s" (Prelude.show lookups)

  ledgerTx <-
    Plutus.Contract.submitTxConstraintsWith lookups tx

  Plutus.Contract.logInfo @Prelude.String PlutusTx.Prelude.$
    Text.Printf.printf "ledgerTx %s" (Prelude.show ledgerTx)

  Data.Functor.void PlutusTx.Prelude.$
    Plutus.Contract.awaitTxConfirmed PlutusTx.Prelude.$ Ledger.getCardanoTxId ledgerTx
  Plutus.Contract.logInfo @Prelude.String PlutusTx.Prelude.$
    Text.Printf.printf
      "made lovelace in auction %s for token (%s, %s)"
      (Prelude.show (tokenCost tokenSaleParam))
      (Prelude.show (Script.currencySymbol tokenSaleParam))
      (Prelude.show (Script.tokenName tokenSaleParam))
  where
    v =
      Plutus.V1.Ledger.Api.singleton
        (Script.currencySymbol tokenSaleParam)
        (Script.tokenName tokenSaleParam)
        1
        PlutusTx.Prelude.<> Ledger.Ada.lovelaceValueOf minLovelace

    isSuitable :: Ledger.ChainIndexTxOut -> PlutusTx.Prelude.Bool
    isSuitable o =
      Value.valueOf
        (Ledger._ciTxOutValue o)
        (Script.currencySymbol tokenSaleParam)
        (Script.tokenName tokenSaleParam)
        PlutusTx.Prelude.>= 1