-- File auto generated by purescript-bridge! --
module Ledger.Address where

import Prelude

import Control.Lazy (defer)
import Data.Argonaut (encodeJson, jsonNull)
import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Decode.Aeson ((</$\>), (</*\>), (</\>))
import Data.Argonaut.Encode (class EncodeJson)
import Data.Argonaut.Encode.Aeson ((>$<), (>/\<))
import Data.Generic.Rep (class Generic)
import Data.Lens (Iso', Lens', Prism', iso, prism')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Show.Generic (genericShow)
import Data.Tuple.Nested ((/\))
import Plutus.V1.Ledger.Crypto (PubKey, PubKeyHash)
import Type.Proxy (Proxy(Proxy))
import Data.Argonaut.Decode.Aeson as D
import Data.Argonaut.Encode.Aeson as E
import Data.Map as Map

newtype PaymentPubKey = PaymentPubKey { unPaymentPubKey :: PubKey }

derive instance Eq PaymentPubKey

derive instance Ord PaymentPubKey

instance Show PaymentPubKey where
  show a = genericShow a

instance EncodeJson PaymentPubKey where
  encodeJson = defer \_ -> E.encode $ unwrap >$< (E.record
                                                 { unPaymentPubKey: E.value :: _ PubKey })

instance DecodeJson PaymentPubKey where
  decodeJson = defer \_ -> D.decode $ (PaymentPubKey <$> D.record "PaymentPubKey" { unPaymentPubKey: D.value :: _ PubKey })

derive instance Generic PaymentPubKey _

derive instance Newtype PaymentPubKey _

--------------------------------------------------------------------------------

_PaymentPubKey :: Iso' PaymentPubKey {unPaymentPubKey :: PubKey}
_PaymentPubKey = _Newtype

--------------------------------------------------------------------------------

newtype PaymentPubKeyHash = PaymentPubKeyHash { unPaymentPubKeyHash :: PubKeyHash }

derive instance Eq PaymentPubKeyHash

derive instance Ord PaymentPubKeyHash

instance Show PaymentPubKeyHash where
  show a = genericShow a

instance EncodeJson PaymentPubKeyHash where
  encodeJson = defer \_ -> E.encode $ unwrap >$< (E.record
                                                 { unPaymentPubKeyHash: E.value :: _ PubKeyHash })

instance DecodeJson PaymentPubKeyHash where
  decodeJson = defer \_ -> D.decode $ (PaymentPubKeyHash <$> D.record "PaymentPubKeyHash" { unPaymentPubKeyHash: D.value :: _ PubKeyHash })

derive instance Generic PaymentPubKeyHash _

derive instance Newtype PaymentPubKeyHash _

--------------------------------------------------------------------------------

_PaymentPubKeyHash :: Iso' PaymentPubKeyHash {unPaymentPubKeyHash :: PubKeyHash}
_PaymentPubKeyHash = _Newtype

--------------------------------------------------------------------------------

newtype StakePubKey = StakePubKey { unStakePubKey :: PubKey }

derive instance Eq StakePubKey

derive instance Ord StakePubKey

instance Show StakePubKey where
  show a = genericShow a

instance EncodeJson StakePubKey where
  encodeJson = defer \_ -> E.encode $ unwrap >$< (E.record
                                                 { unStakePubKey: E.value :: _ PubKey })

instance DecodeJson StakePubKey where
  decodeJson = defer \_ -> D.decode $ (StakePubKey <$> D.record "StakePubKey" { unStakePubKey: D.value :: _ PubKey })

derive instance Generic StakePubKey _

derive instance Newtype StakePubKey _

--------------------------------------------------------------------------------

_StakePubKey :: Iso' StakePubKey {unStakePubKey :: PubKey}
_StakePubKey = _Newtype

--------------------------------------------------------------------------------

newtype StakePubKeyHash = StakePubKeyHash { unStakePubKeyHash :: PubKeyHash }

derive instance Eq StakePubKeyHash

derive instance Ord StakePubKeyHash

instance Show StakePubKeyHash where
  show a = genericShow a

instance EncodeJson StakePubKeyHash where
  encodeJson = defer \_ -> E.encode $ unwrap >$< (E.record
                                                 { unStakePubKeyHash: E.value :: _ PubKeyHash })

instance DecodeJson StakePubKeyHash where
  decodeJson = defer \_ -> D.decode $ (StakePubKeyHash <$> D.record "StakePubKeyHash" { unStakePubKeyHash: D.value :: _ PubKeyHash })

derive instance Generic StakePubKeyHash _

derive instance Newtype StakePubKeyHash _

--------------------------------------------------------------------------------

_StakePubKeyHash :: Iso' StakePubKeyHash {unStakePubKeyHash :: PubKeyHash}
_StakePubKeyHash = _Newtype
