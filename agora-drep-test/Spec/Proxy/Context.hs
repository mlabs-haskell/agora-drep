module Spec.Proxy.Context (spendingContextSpec, mintingContextSpec) where

import Agora.Proxy (ProxyDatum (ProxyDatum, pdDatumHash, pdReceiverScript))
import Crypto.Hash (
  Blake2b_224 (Blake2b_224),
  hashWith,
 )

import Data.ByteArray qualified as ByteArray
import Data.ByteString (ByteString)
import Data.ByteString.Short qualified as ByteStringS
import Plutarch.LedgerApi.V3 (datumHash)
import Plutarch.Script (Script (Script), unScript)
import Plutarch.Test.Program (ScriptCase (ScriptCase), ScriptResult (ScriptFailure, ScriptSuccess), testScript)
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
import PlutusCore qualified as PLC
import PlutusLedgerApi.V1.Value qualified as Value
import PlutusLedgerApi.V3 (
  BuiltinByteString,
  Credential (ScriptCredential),
  CurrencySymbol (CurrencySymbol),
  Datum (Datum),
  ScriptContext,
  ScriptHash (ScriptHash),
  ToData (toBuiltinData),
  TokenName (TokenName),
  TxCert (TxCertRegStaking),
  serialiseUPLC,
  toData,
 )
import PlutusTx.Prelude (toBuiltin)
import Test.Tasty (TestTree, testGroup)
import UntypedPlutusCore (Program (Program), Term (Apply, Constant))

data TestConfig = TestConfig
  { gat3Credential :: Credential
  , gat3CurSym :: CurrencySymbol
  , ownScript :: Script
  }

testConfigFromScript :: Script -> TestConfig
testConfigFromScript gat3Script =
  let gat3ScriptHash = scriptHash gat3Script
      ScriptHash scriptHashBS = gat3ScriptHash
   in TestConfig
        { gat3Credential = ScriptCredential gat3ScriptHash
        , gat3CurSym = CurrencySymbol scriptHashBS
        , ownScript = gat3Script
        }

-- | Unit tests
spendingContextSpec :: Script -> TestTree
spendingContextSpec gat3Script =
  let
    config = testConfigFromScript gat3Script
    mkTest' = mkTest config
   in
    testGroup
      "Context tests"
      [ mkTest' "OK case: Valid GAT2 spend" validGAT2Spend ScriptSuccess
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

-- | Unit tests
mintingContextSpec :: Script -> TestTree
mintingContextSpec gat3Script =
  let
    config = testConfigFromScript gat3Script
    mkTest' = mkTest config
   in
    testGroup
      "Context tests"
      [ mkTest' "OK case: Valid GAT3 mint" validGAT3Mint ScriptSuccess
      , mkTest' "Fail case: Minting more than one GAT v3" mintMoreThan1Gat3' ScriptFailure
      , mkTest' "Fail case: GAT v3 multiple tokens with different token names" multipleTokenNames ScriptFailure
      , mkTest' "Fail case: GAT v3 token name not empty" nonEmptyTokenName ScriptFailure
      ]

-- * ScriptContexts for the test cases

validGAT2Spend :: TestConfig -> ScriptContext
validGAT2Spend config =
  buildSpending' $
    mconcat
      [ withSpendingUTXO (gat2Utxo config)
      , input (gat2Utxo config)
      , output (gat3Utxo config)
      , mint (Value.singleton gat2CurSym (TokenName "") (-1))
      , mint (Value.singleton (gat3CurSym config) (TokenName "") 1)
      ]

missingGat2Burn :: TestConfig -> ScriptContext
missingGat2Burn config =
  buildSpending' $
    mconcat
      [ withSpendingUTXO (gat2Utxo config)
      , input (gat2Utxo config)
      , output (gat3Utxo config)
      , mint (Value.singleton (gat3CurSym config) (TokenName "") 1)
      ]

missingGat2FromUtxo :: TestConfig -> ScriptContext
missingGat2FromUtxo config =
  buildSpending' $
    mconcat
      [ output (gat3Utxo config)
      , mint (Value.singleton gat2CurSym (TokenName "") (-1))
      , mint (Value.singleton (gat3CurSym config) (TokenName "") 1)
      ]

mintMoreThan1Gat3 :: TestConfig -> ScriptContext
mintMoreThan1Gat3 config =
  buildSpending' $
    mconcat
      [ withSpendingUTXO (gat2Utxo config)
      , input (gat2Utxo config)
      , output (gat3Utxo config)
      , mint (Value.singleton gat2CurSym (TokenName "") (-1))
      , mint (Value.singleton (gat3CurSym config) (TokenName "") 2)
      ]

missingReceiverOutput :: TestConfig -> ScriptContext
missingReceiverOutput config =
  buildSpending' $
    mconcat
      [ withSpendingUTXO (gat2Utxo config)
      , input (gat2Utxo config)
      , mint (Value.singleton gat2CurSym (TokenName "") (-1))
      , mint (Value.singleton (gat3CurSym config) (TokenName "") 1)
      ]

invalidGAT3Datum :: TestConfig -> ScriptContext
invalidGAT3Datum config =
  buildSpending' $
    mconcat
      [ withSpendingUTXO (gat2Utxo config)
      , input (gat2Utxo config)
      , output (invalidGat3Utxo config)
      , mint (Value.singleton gat2CurSym (TokenName "") (-1))
      , mint (Value.singleton (gat3CurSym config) (TokenName "") 1)
      ]

mint3rdToken :: TestConfig -> ScriptContext
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

includesCerts :: TestConfig -> ScriptContext
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

includesOtherScripts :: TestConfig -> ScriptContext
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

validGAT3Mint :: TestConfig -> ScriptContext
validGAT3Mint config =
  buildMinting' $
    mconcat
      [ withMinting (gat3CurSym config)
      , input (gat2Utxo config)
      , mint (Value.singleton (gat3CurSym config) (TokenName "") 1)
      ]

mintMoreThan1Gat3' :: TestConfig -> ScriptContext
mintMoreThan1Gat3' config =
  buildMinting' $
    mconcat
      [ withMinting (gat3CurSym config)
      , input (gat2Utxo config)
      , mint (Value.singleton (gat3CurSym config) (TokenName "") 2)
      ]

multipleTokenNames :: TestConfig -> ScriptContext
multipleTokenNames config =
  buildMinting' $
    mconcat
      [ withMinting (gat3CurSym config)
      , input (gat2Utxo config)
      , mint (Value.singleton (gat3CurSym config) (TokenName "oo-wee") 1)
      , mint (Value.singleton (gat3CurSym config) (TokenName "") 1)
      ]

nonEmptyTokenName :: TestConfig -> ScriptContext
nonEmptyTokenName config =
  buildMinting' $
    mconcat
      [ withMinting (gat3CurSym config)
      , input (gat2Utxo config)
      , mint (Value.singleton (gat3CurSym config) (TokenName "oo-wee") 1)
      ]

-- * Building blocks for the test ScriptContexts

-- | Hash of the receiver script of the GAT v3 token
receiverScriptHash :: ScriptHash
receiverScriptHash = ScriptHash "001122"

-- | Address credential of the receiver script of the Proxy token
receiverCredential :: Credential
receiverCredential = ScriptCredential receiverScriptHash

-- | Datum attached to the GAT v3 token's UTxO
receiverDatum :: Integer
receiverDatum = 112233

-- | Proxy Datum, attached to the GAT v2 token's UTxO
proxyDatum :: ProxyDatum
proxyDatum =
  ProxyDatum
    { pdReceiverScript = receiverScriptHash
    , pdDatumHash = datumHash . Datum . toBuiltinData $ receiverDatum
    }

-- | GAT v2 currency symbol
gat2CurSym :: CurrencySymbol
gat2CurSym = CurrencySymbol "aabbcc"

-- | UTxO containing GAT v2 token
gat2Utxo :: TestConfig -> UTXO
gat2Utxo config =
  mconcat
    [ withValue (Value.singleton gat2CurSym (TokenName "") 1)
    , withInlineDatum proxyDatum
    , withCredential (gat3Credential config)
    ]

-- | UTxO containing GAT v3 token
gat3Utxo :: TestConfig -> UTXO
gat3Utxo config =
  mconcat
    [ withValue (Value.singleton (gat3CurSym config) (TokenName "") 1)
    , withHashDatum receiverDatum
    , withCredential receiverCredential
    ]

-- | UTxO containing GAT v3 token
invalidGat3Utxo :: TestConfig -> UTXO
invalidGat3Utxo config =
  mconcat
    [ withValue (Value.singleton (gat3CurSym config) (TokenName "") 1)
    , withHashDatum (01234 :: Integer)
    , withCredential receiverCredential
    ]

-- * Test utilities

mkTest :: TestConfig -> String -> (TestConfig -> ScriptContext) -> ScriptResult -> TestTree
mkTest config testName toContext expectedResult =
  let script = ownScript config
      context = toContext config
      Script applied = uncheckedApplyDataToScript (context) $ uncheckedApplyDataToScript gat2CurSym script
   in testScript $ ScriptCase testName expectedResult applied applied

uncheckedApplyDataToScript :: (ToData argument) => argument -> Script -> Script
uncheckedApplyDataToScript argument (Script (Program () version unappliedTerm)) =
  Script
    . Program () version
    . Apply () unappliedTerm
    . Constant ()
    . PLC.someValue
    $ toData argument

-- TODO: Remove this once Plutarch scriptHash function is fixed
scriptHash :: Script -> ScriptHash
scriptHash = hashScriptWithPrefix "\x03"

-- TODO: Remove this once Plutarch scriptHash function is fixed
hashScriptWithPrefix :: ByteString -> Script -> ScriptHash
hashScriptWithPrefix prefix scr =
  ScriptHash . hashBlake2b_224 $
    prefix <> (ByteStringS.fromShort . serialiseUPLC . unScript $ scr)

-- TODO: Remove this once Plutarch scriptHash function is fixed
hashBlake2b_224 :: ByteString -> BuiltinByteString
hashBlake2b_224 = toBuiltin . ByteArray.convert @_ @ByteString . hashWith Blake2b_224
