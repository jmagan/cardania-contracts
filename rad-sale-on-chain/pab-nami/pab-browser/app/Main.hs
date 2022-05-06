{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main
  ( main,
    writeTPS,
    tokenSaleParamExample,
    test,
    getAddressTSPExample,
    getAddressExample

  )
where

import qualified Plutus.PAB.Effects.Contract.Builtin
import qualified Plutus.PAB.Run
import qualified RadSaleContracts
import qualified Prelude
import Prelude
import Script
import           Cardano.Api
import           Cardano.Api.Shelley   (PlutusScript (..))
import           Codec.Serialise       (serialise)
import           Data.Aeson            (encode)
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.ByteString.Short as SBS
import           Data.Text             as T
import           PlutusTx              (Data (..))
import qualified PlutusTx
import qualified Ledger
import Plutus.V1.Ledger.Value as Value
import Plutus.V1.Ledger.Scripts as Scripts

main :: Prelude.IO ()
main = do
  Plutus.PAB.Run.runWith (Plutus.PAB.Effects.Contract.Builtin.handleBuiltin @RadSaleContracts.RadSaleContracts)

tokenSaleParamExample :: TokenSaleParam
tokenSaleParamExample = TokenSaleParam
    { Script.tokenCost = 100,
      Script.currencySymbol = "641593ca39c5cbd3eb314533841d53e61ebf6ee7a0ec7c391652f31e",
      Script.tokenName = Value.tokenName "CardaniaFounderWhite",
      Script.sellerPubKeyHash = "84fab74abaff1d265aaf2110cd8185015a19aef93ca271cda261fd32"
    }

getAddressTSPExample :: Ledger.Address
getAddressTSPExample = Script.scrAddress tokenSaleParamExample

getAddressExample :: Ledger.Address
getAddressExample = Ledger.scriptHashAddress $ Scripts.ValidatorHash "ij\221\132\152B5<\216;\155\201a\STX\239\158\213z\196\225N\195\174%\\\212\&7\198"

writeJSON :: FilePath -> TokenSaleParam -> IO ()
writeJSON file = LBS.writeFile file . encode 

writeTPS :: IO ()
writeTPS = writeJSON "tps.json" tokenSaleParamExample

shelleyAddressType :: AsType ShelleyEra
shelleyAddressType = AsShelleyEra

textBech32 :: T.Text
textBech32 = T.pack "addr_test1qzz04d62htl36fj64us3pnvps5q45xdwly72yuwd5fsl6v5w85yj9tjv9rajy5rgsltwtehhr9lzrftkptjke2nhg0tqs6s9us"

test :: Maybe AddressAny 
test = deserialiseAddress AsAddressAny textBech32

-- Keep this here for now. Eventually, This function will call the `migrate`
-- command before running the webserver.
--
-- let opts = AppOpts{minLogLevel = Nothing, logConfigPath = Nothing, configPath = Nothing, runEkgServer = False, storageBackend = BeamSqliteBackend, cmd = PABWebserver, PAB.Command.passphrase = Nothing}
--     networkID = NetworkIdWrapper $ CAPI.Testnet $ CAPI.NetworkMagic 1097911063
--     config = PAB.Config.defaultConfig
--         { nodeServerConfig = def{mscNodeMode=AlonzoNode,mscNetworkId=networkID} -- def{mscSocketPath=nodeSocketFile socketPath,mscNodeMode=AlonzoNode,mscNetworkId=networkID}
--         , dbConfig = def{dbConfigFile = "plutus-pab.db"} -- def{dbConfigFile = T.pack (dir </> "plutus-pab.db")}
--         , chainIndexConfig = def -- def{PAB.CI.ciBaseUrl = PAB.CI.ChainIndexUrl $ BaseUrl Http "localhost" chainIndexPort ""}
--         , walletServerConfig = def -- def{Wallet.Config.baseUrl=WalletUrl walletUrl}
--         }

-- void . async $ runWithOpts @DemoContract handleBuiltin (Just config) opts{cmd=Migrate}
-- sleep 2
-- void . async $ runWithOpts @DemoContract handleBuiltin (Just config) opts{cmd=PABWebserver}

-- -- Pressing enter stops the server
-- void getLine

-- sleep :: Int -> IO ()
-- sleep n = threadDelay $ n * 1_000_000
