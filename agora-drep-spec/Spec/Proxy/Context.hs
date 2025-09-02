{- | Proxy script unit tests

@since 1.0.0
-}
module Spec.Proxy.Context (
  spendingContextSpec,
  mintingContextSpec,
  validGAT2Spend,
  validGAT3Mint,
) where

import Agora.Proxy (ProxyDatum (ProxyDatum, pdDatumHash, pdReceiverScript))
import Plutarch.LedgerApi.V3 (datumHash, scriptHash)
import Plutarch.Script (Script)
import Plutarch.Test.Program (ScriptResult (ScriptFailure, ScriptSuccess))
import Plutus.ContextBuilder (
  UTXO,
  buildMinting',
  buildSpending',
  input,
  mint,
  output,
  txCert,
  withCredential,
  withHashDatum,
  withInlineDatum,
  withMinting,
  withSpendingUTXO,
  withValue,
 )
import PlutusLedgerApi.V1.Value qualified as Value
import PlutusLedgerApi.V3 (
  Credential (ScriptCredential),
  CurrencySymbol (CurrencySymbol),
  Datum (Datum),
  ScriptContext,
  ScriptHash (ScriptHash),
  ToData (toBuiltinData),
  TokenName (TokenName),
  TxCert (TxCertRegStaking),
 )
import Spec.Utils (TestConfig (ownScript, testConfigFromScript), mkTest, uncheckedApplyDataToScript)
import Test.Tasty (TestTree, testGroup)

{- | Context for the test case

@since 1.0.0
-}
data TestConfigProxy = TestConfigProxy
  { gat3Credential :: Credential
  , gat3CurSym :: CurrencySymbol
  , gat3Script :: Script
  }

-- | @since 1.0.0
instance TestConfig TestConfigProxy where
  ownScript config = uncheckedApplyDataToScript gat2CurSym (gat3Script config)

  testConfigFromScript gat3Script =
    let gat3ScriptHash = scriptHash gat3Script
        ScriptHash scriptHashBS = gat3ScriptHash
     in TestConfigProxy
          { gat3Credential = ScriptCredential gat3ScriptHash
          , gat3CurSym = CurrencySymbol scriptHashBS
          , gat3Script
          }

{- | Proxy script unit tests for spending cases

@since 1.0.0
-}
spendingContextSpec :: Script -> TestTree
spendingContextSpec gat3Script =
  let
    config = testConfigFromScript gat3Script
    mkTest' = mkTest config
   in
    testGroup
      "Spending Context tests"
      [ mkTest' "OK case: Valid GAT2 spend" validGAT2Spend ScriptSuccess
      , mkTest' "Fail case: Invalid V3 name" invalidV3Name ScriptFailure
      , mkTest' "Fail case: Missing GAT v2 burn" missingGat2Burn ScriptFailure
      , mkTest' "Fail case: Missing GAT v2 token from spent UTxO" missingGat2FromUtxo ScriptFailure
      , mkTest' "Fail case: Minting more than one GAT v3" mintMoreThan1Gat3 ScriptFailure
      , mkTest' "Fail case: Missing GAT v3 token from spent UTxO" missingGat2FromUtxo ScriptFailure
      , mkTest' "Fail case: Missing output at receiver address" missingReceiverOutput ScriptFailure
      , mkTest' "Fail case: Invalid datum in receiver UTxO" invalidGAT3Datum ScriptFailure
      , mkTest' "Fail case: Mint unknown token alongside GATs" mint3rdToken ScriptFailure
      , mkTest' "Fail case: Transaction includes certificates" includesCerts ScriptFailure
      , mkTest' "Fail case: Transaction includes script input other than own input" includesOtherScripts ScriptFailure
      ]

{- | Proxy script unit tests for minting cases

@since 1.0.0
-}
mintingContextSpec :: Script -> TestTree
mintingContextSpec gat3Script =
  let
    config = testConfigFromScript gat3Script
    mkTest' = mkTest config
   in
    testGroup
      "Minting Context tests"
      [ mkTest' "OK case: Valid GAT3 mint" validGAT3Mint ScriptSuccess
      , mkTest' "OK case: Valid GAT3 burn" validGAT3Burn ScriptSuccess
      , mkTest' "Fail case: Minting without spending own input" mintWithoutSpend ScriptFailure
      ]

-- * ScriptContexts for the test cases

{- | Valid Governcance Authority Token spending (should pass)

@since 1.0.0
-}
validGAT2Spend :: TestConfigProxy -> ScriptContext
validGAT2Spend config =
  buildSpending' $
    mconcat
      [ withSpendingUTXO (gat2Utxo config)
      , input (gat2Utxo config)
      , output (gat3Utxo config)
      , mint (Value.singleton gat2CurSym (TokenName "") (-1))
      , mint (Value.singleton (gat3CurSym config) (TokenName "") 1)
      ]

{- | Attempt to spend Proxied Governance Authority Token with a non-empty name
(should fail)

@since 1.0.0
-}
invalidV3Name :: TestConfigProxy -> ScriptContext
invalidV3Name config =
  buildSpending' $
    mconcat
      [ withSpendingUTXO (gat2Utxo config)
      , input (gat2Utxo config)
      , output (gat3Utxo config)
      , mint (Value.singleton gat2CurSym (TokenName "") (-1))
      , mint (Value.singleton (gat3CurSym config) (TokenName "aabbcc") 1)
      ]

{- | Attempt to spend Proxied Governance Authority Token without burning its V2
counterpart (should fail)

@since 1.0.0
-}
missingGat2Burn :: TestConfigProxy -> ScriptContext
missingGat2Burn config =
  buildSpending' $
    mconcat
      [ withSpendingUTXO (gat2Utxo config)
      , input (gat2Utxo config)
      , output (gat3Utxo config)
      , mint (Value.singleton (gat3CurSym config) (TokenName "") 1)
      ]

{- | Attempt to spend Proxied Governance Authority Token without including its
V2 counterpart in the transaction(should fail)

@since 1.0.0
-}
missingGat2FromUtxo :: TestConfigProxy -> ScriptContext
missingGat2FromUtxo config =
  buildSpending' $
    mconcat
      [ output (gat3Utxo config)
      , mint (Value.singleton gat2CurSym (TokenName "") (-1))
      , mint (Value.singleton (gat3CurSym config) (TokenName "") 1)
      ]

{- | Attempt to mint more than one Proxied Governance Authority Tokens
(should fail)

@since 1.0.0
-}
mintMoreThan1Gat3 :: TestConfigProxy -> ScriptContext
mintMoreThan1Gat3 config =
  buildSpending' $
    mconcat
      [ withSpendingUTXO (gat2Utxo config)
      , input (gat2Utxo config)
      , output (gat3Utxo config)
      , mint (Value.singleton gat2CurSym (TokenName "") (-1))
      , mint (Value.singleton (gat3CurSym config) (TokenName "") 2)
      ]

{- | Attempt to mint a Proxied Governance Authority Token without sending it to
the designated address (should fail)

@since 1.0.0
-}
missingReceiverOutput :: TestConfigProxy -> ScriptContext
missingReceiverOutput config =
  buildSpending' $
    mconcat
      [ withSpendingUTXO (gat2Utxo config)
      , input (gat2Utxo config)
      , mint (Value.singleton gat2CurSym (TokenName "") (-1))
      , mint (Value.singleton (gat3CurSym config) (TokenName "") 1)
      ]

{- | Attempt to send a Proxied Governance Authority Token with a datum not
matching the hash designated by the V2 GAT (should fail)

@since 1.0.0
-}
invalidGAT3Datum :: TestConfigProxy -> ScriptContext
invalidGAT3Datum config =
  buildSpending' $
    mconcat
      [ withSpendingUTXO (gat2Utxo config)
      , input (gat2Utxo config)
      , output (invalidGat3Utxo config)
      , mint (Value.singleton gat2CurSym (TokenName "") (-1))
      , mint (Value.singleton (gat3CurSym config) (TokenName "") 1)
      ]

{- | Attempt to mint other unknown tokens in the transaction (should fail)

@since 1.0.0
-}
mint3rdToken :: TestConfigProxy -> ScriptContext
mint3rdToken config =
  buildSpending' $
    mconcat
      [ withSpendingUTXO (gat2Utxo config)
      , input (gat2Utxo config)
      , output (gat3Utxo config)
      , mint (Value.singleton gat2CurSym (TokenName "") (-1))
      , mint (Value.singleton (gat3CurSym config) (TokenName "") 1)
      , mint (Value.singleton (CurrencySymbol "aabbccdd1234") (TokenName "") 1)
      ]

{- | Attempt to include certificates in the transaction (should fail)

@since 1.0.0
-}
includesCerts :: TestConfigProxy -> ScriptContext
includesCerts config =
  buildSpending' $
    mconcat
      [ withSpendingUTXO (gat2Utxo config)
      , input (gat2Utxo config)
      , output (gat3Utxo config)
      , mint (Value.singleton gat2CurSym (TokenName "") (-1))
      , mint (Value.singleton (gat3CurSym config) (TokenName "") 1)
      , txCert (TxCertRegStaking (gat3Credential config) (Just 5000000))
      ]

{- | Attempt to include other unknown Plutus scripts in the transaction
(should fail)

@since 1.0.0
-}
includesOtherScripts :: TestConfigProxy -> ScriptContext
includesOtherScripts config =
  buildSpending' $
    mconcat
      [ withSpendingUTXO (gat2Utxo config)
      , input (gat2Utxo config)
      , input
          ( mconcat
              [ withCredential (ScriptCredential "deadbeef")
              ]
          )
      , output (gat3Utxo config)
      , mint (Value.singleton gat2CurSym (TokenName "") (-1))
      , mint (Value.singleton (gat3CurSym config) (TokenName "") 1)
      ]

{- | Valid Governance Authority Token mint (should pass)

@since 1.0.0
-}
validGAT3Mint :: TestConfigProxy -> ScriptContext
validGAT3Mint config =
  buildMinting' $
    mconcat
      [ withMinting (gat3CurSym config)
      , input (gat2Utxo config)
      , mint (Value.singleton (gat3CurSym config) (TokenName "") 1)
      , mint (Value.singleton gat2CurSym (TokenName "") (-1))
      ]

{- | Valid Governance Authority Token burn (should pass)

@since 1.0.0
-}
validGAT3Burn :: TestConfigProxy -> ScriptContext
validGAT3Burn config =
  buildMinting' $
    mconcat
      [ withMinting (gat3CurSym config)
      , mint (Value.singleton (gat3CurSym config) (TokenName "") (-1))
      ]

{- | Attempt to mint a Governance Authority Token with the spending validator
failing (should fail)

@since 1.0.0
-}
mintWithoutSpend :: TestConfigProxy -> ScriptContext
mintWithoutSpend config =
  buildMinting' $
    mconcat
      [ withMinting (gat3CurSym config)
      , mint (Value.singleton (gat3CurSym config) (TokenName "") 1)
      ]

-- * Building blocks for the test ScriptContexts

{- | Hash of the receiver script of the GAT v3 token

@since 1.0.0
-}
receiverScriptHash :: ScriptHash
receiverScriptHash = ScriptHash "001122"

{- | Address credential of the receiver script of the Proxy token

@since 1.0.0
-}
receiverCredential :: Credential
receiverCredential = ScriptCredential receiverScriptHash

{- | Datum attached to the GAT v3 token's UTxO

@since 1.0.0
-}
receiverDatum :: Integer
receiverDatum = 112233

{- | Proxy Datum, attached to the GAT v2 token's UTxO

@since 1.0.0
-}
proxyDatum :: ProxyDatum
proxyDatum =
  ProxyDatum
    { pdReceiverScript = receiverScriptHash
    , pdDatumHash = datumHash . Datum . toBuiltinData $ receiverDatum
    }

{- | GAT v2 currency symbol

@since 1.0.0
-}
gat2CurSym :: CurrencySymbol
gat2CurSym = CurrencySymbol "aabbcc"

{- | UTxO containing GAT v2 token

@since 1.0.0
-}
gat2Utxo :: TestConfigProxy -> UTXO
gat2Utxo config =
  mconcat
    [ withValue (Value.singleton gat2CurSym (TokenName "") 1)
    , withInlineDatum proxyDatum
    , withCredential (gat3Credential config)
    ]

{- | UTxO containing GAT v3 token

@since 1.0.0
-}
gat3Utxo :: TestConfigProxy -> UTXO
gat3Utxo config =
  mconcat
    [ withValue (Value.singleton (gat3CurSym config) (TokenName "") 1)
    , withHashDatum receiverDatum
    , withCredential receiverCredential
    ]

{- | UTxO containing GAT v3 token

@since 1.0.0
-}
invalidGat3Utxo :: TestConfigProxy -> UTXO
invalidGat3Utxo config =
  mconcat
    [ withValue (Value.singleton (gat3CurSym config) (TokenName "") 1)
    , withHashDatum (01234 :: Integer)
    , withCredential receiverCredential
    ]
