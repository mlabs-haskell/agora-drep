module Agora.Drep.Test.Proxy (spec) where

import Agora.Proxy (ProxyDatum (ProxyDatum), proxyScript)
import Data.Either (fromRight)
import Plutarch.Internal.Term (
    Config (Tracing),
    LogLevel (LogDebug),
    Script (Script),
    TracingMode (DoTracingAndBinds),
    compile,
 )
import Plutarch.LedgerApi.V3 (datumHash, scriptHash)
import Plutarch.Test.Program (ScriptCase (ScriptCase), ScriptResult (ScriptSuccess), testScript)
import Plutus.ContextBuilder (
    SpendingBuilder,
    UTXO,
    address,
    buildSpending',
    input,
    mint,
    output,
    withHashDatum,
    withInlineDatum,
    withSpendingUTXO,
    withValue,
 )
import PlutusCore qualified as PLC
import PlutusLedgerApi.V1.Address (scriptHashAddress)
import PlutusLedgerApi.V1.Value qualified as Value
import PlutusLedgerApi.V3 (
    CurrencySymbol (CurrencySymbol),
    Datum (Datum),
    Redeemer (Redeemer),
    ScriptHash (getScriptHash),
    ToData (toBuiltinData),
    TokenName (TokenName),
    toData,
 )
import Test.Tasty (TestName, TestTree, testGroup)
import UntypedPlutusCore (Program (Program), Term (Apply, Constant))

spec :: TestTree
spec =
    testGroup
        "Proxy"
        [ mkProxyTest
            "Valid"
            ScriptSuccess
            [ input gatInput
            , withSpendingUTXO gatInput
            , output gatOutput
            , mint $ Value.singleton gatSymbolV2 (TokenName "") (-1)
            , mint $ Value.singleton gatSymbolV3 (TokenName "") 1
            ]
        ]

gatInput :: UTXO
gatInput =
    mconcat
        [ withInlineDatum $ ProxyDatum validReceiver (datumHash $ Datum $ toBuiltinData expectedDatum)
        , withValue $ Value.singleton gatSymbolV2 (TokenName "") 1
        , address $ scriptHashAddress $ scriptHash appliedDebug
        ]

gatOutput :: UTXO
gatOutput =
    mconcat
        [ address $ scriptHashAddress validReceiver
        , withValue $ Value.singleton gatSymbolV3 (TokenName "") 1
        , withHashDatum expectedDatum
        ]

gatSymbolV2 :: CurrencySymbol
gatSymbolV2 = CurrencySymbol "00000000000000000000000000000000000000000000000000000000"

gatSymbolV3 :: CurrencySymbol
gatSymbolV3 = CurrencySymbol $ getScriptHash $ scriptHash appliedDebug

validReceiver :: ScriptHash
validReceiver = "11111111111111111111111111111111111111111111111111111111"

expectedDatum :: Integer
expectedDatum = 42

mkProxyTest :: TestName -> ScriptResult -> [SpendingBuilder] -> TestTree
mkProxyTest name result builder =
    let context = buildSpending' (Redeemer $ toBuiltinData ()) $ mconcat builder
        Script debug = uncheckedApplyDataToScript appliedDebug context
     in testScript $ ScriptCase name result debug debug

appliedDebug :: Script
appliedDebug = uncheckedApplyDataToScript rawDebug gatSymbolV2

rawDebug :: Script
rawDebug = fromRight undefined $ compile (Tracing LogDebug DoTracingAndBinds) proxyScript

uncheckedApplyDataToScript :: (ToData argument) => Script -> argument -> Script
uncheckedApplyDataToScript (Script (Program () version unappliedTerm)) argument =
    Script
        . Program () version
        . Apply () unappliedTerm
        . Constant ()
        . PLC.someValue
        $ toData argument
