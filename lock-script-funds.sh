cardano-cli query protocol-parameters --testnet-magic 1097911063 --out-file ../swap-on-chain/transactions/protocol.json

utxovkey=../octoberFestMetaData/minting.vkey
utxoskey=../octoberFestMetaData/minting.skey

cardano-cli address build --payment-script-file ../swap-on-chain/transactions/result.plutus --testnet-magic 1097911063 --out-file ../swap-on-chain/transactions/script.addr

paymentAddress="addr_test1vrh0kkuahtz28qpfdhsx2hm2eekf06des8h03xnm757u65sd6egwy"
scriptAddress="addr_test1wqptunx4y74mryhln9qlxelqzd8dxhg4efzqc6ayahetmggxszk7u"
scriptTxOut="$scriptAddress+10000000"
cardano-cli transaction hash-script-data --script-data-value 0
scriptdatumhash="d36a2619a672494604e11bb447cbcf5231e9f2ba25c2169177edc941bd50ad6c"

paymentAddressUtxo="4d3148320e8b7594915541332ee4b365ad2197f6fe3581e56920a11e22ebe960#0"

cardano-cli transaction build \
  --alonzo-era \
  --cardano-mode \
  --testnet-magic 1097911063 \
  --tx-in "$paymentAddressUtxo" \
  --tx-out "$scriptTxOut" \
  --tx-out-datum-hash ${scriptdatumhash} \
  --change-address "$paymentAddress" \
  --protocol-params-file ~/Documents/swap-on-chain/transactions/protocol.json \
  --out-file "../swap-on-chain/transactions/plutusSubmit.body"

cardano-cli transaction sign \
  --tx-body-file "../swap-on-chain/transactions/plutusSubmit.body" \
  --mainnet \
  --signing-key-file "$utxoskey" \
  --out-file "../swap-on-chain/transactions/plutusSubmit.tx"

cardano-cli transaction submit --tx-file "../swap-on-chain/transactions/plutusSubmit.tx" --testnet-magic 1097911063
