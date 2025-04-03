module Main (main) where

import Agora.Effects.Voting (votingEffectScript)
import Agora.Proxy (proxyScript)
import Data.Kind (Type)
import Plutarch.Internal.Term (
  Config (NoTracing),
  RawTerm (RCompiled),
  S,
  Term (Term),
  TermResult (TermResult),
  compile,
 )
import Plutarch.Script (Script (Script))
import Plutarch.Test.Bench (bench, defaultMain)
import PlutusLedgerApi.V3 (CurrencySymbol (CurrencySymbol), ScriptContext)
import Spec.Effects.Voting.Context qualified as Voting
import Spec.Proxy.Context qualified as Proxy
import Spec.Utils (TestConfig (testConfigFromScript), uncheckedApplyDataToScript)
import Test.Tasty (TestName, TestTree, testGroup)
import UntypedPlutusCore (Program (_progTerm))

main :: IO ()
main =
  defaultMain $
    testGroup
      "Benchmarks"
      [ benchScript "GAT V2 Spend" compiledProxyScript Proxy.validGAT2Spend
      , benchScript "GAT V3 Mint" compiledProxyScript Proxy.validGAT3Mint
      , benchScript "Voting Effect Certify" compiledVotingScript Voting.validCert
      , benchScript "Voting Effect Spend" compiledVotingScript Voting.validSpend
      , benchScript "Voting Effect Vote" compiledVotingScript Voting.validVote
      ]

benchScript :: (TestConfig c) => TestName -> Script -> (c -> ScriptContext) -> TestTree
benchScript name script mkContext =
  let
    gat2CurSym :: CurrencySymbol
    gat2CurSym = CurrencySymbol "aabbcc"
   in
    bench
      name
      (unsafeTermFromScript (uncheckedApplyDataToScript (mkContext (testConfigFromScript script)) $ uncheckedApplyDataToScript gat2CurSym script))

compiledProxyScript :: Script
compiledProxyScript =
  either (error . show) id $
    compile NoTracing proxyScript

compiledVotingScript :: Script
compiledVotingScript =
  either (error . show) id $
    compile NoTracing votingEffectScript

unsafeTermFromScript :: forall (p :: S -> Type). Script -> (forall (s :: S). Term s p)
unsafeTermFromScript (Script script) =
  Term $ const $ pure $ TermResult (RCompiled $ _progTerm script) []
