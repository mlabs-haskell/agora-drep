module Spec.Effects.Voting.Context (certifyingContextSpec, votingContextSpec, spendingContextSpec) where

import Agora.Effects.Voting (VotingDatum (VotingDatum, vdGovernanceActionId, vdVote))
import Crypto.Hash (Blake2b_224 (Blake2b_224), hashWith)
import Data.ByteArray qualified as ByteArray
import Data.ByteString (ByteString)
import Data.ByteString.Short qualified as ByteStringS
import Plutarch.Script (Script (Script), unScript)
import Plutarch.Test.Program (ScriptCase (ScriptCase), ScriptResult (ScriptFailure, ScriptSuccess), testScript)
import Plutus.ContextBuilder (UTXO, buildCertifying', buildSpending', buildVoting', input, mint, proposalProcedure, treasuryDonation, txCert, vote, withCertifying, withCredential, withHashDatum, withSpendingUTXO, withValue, withVoting)
import PlutusCore qualified as PLC
import PlutusLedgerApi.V1.Value qualified as Value
import PlutusLedgerApi.V3 (BuiltinByteString, Credential (ScriptCredential), CurrencySymbol (CurrencySymbol), DRepCredential (DRepCredential), GovernanceAction (InfoAction), GovernanceActionId (GovernanceActionId), Lovelace (Lovelace), ProposalProcedure (ProposalProcedure), ScriptContext, ScriptHash (ScriptHash), ToData, TokenName (TokenName), TxId (TxId), Vote (VoteNo, VoteYes), serialiseUPLC, toBuiltin, toData)
import PlutusLedgerApi.V3.Contexts (TxCert (TxCertRegDRep, TxCertRegStaking), Voter (DRepVoter))
import Test.Tasty (TestTree, testGroup)
import UntypedPlutusCore (Program (Program), Term (Apply, Constant))

data TestConfig = TestConfig
  { vsCredential :: Credential
  , ownScript :: Script
  }

testConfigFromScript :: Script -> TestConfig
testConfigFromScript votingScript =
  let votingScriptHash = scriptHash votingScript
   in TestConfig
        { vsCredential = ScriptCredential votingScriptHash
        , ownScript = votingScript
        }

-- | Unit tests
certifyingContextSpec :: Script -> TestTree
certifyingContextSpec votingScript =
  let
    config = testConfigFromScript votingScript
    mkTest' = mkTest config
   in
    testGroup
      "Certifying Context tests"
      [ mkTest' "OK case: Valid certifying transaction" validCert ScriptSuccess
      , mkTest' "Fail case: transaction contains votes" hasVotes ScriptFailure
      , mkTest' "Fail case: transaction contains more than one tx certs" multipleTxCerts ScriptFailure
      , mkTest' "Fail case: transaction contains proposal procedures" hasPProcs ScriptFailure
      , mkTest' "Fail case: transaction contains treasury donations" hasTrDonations ScriptFailure
      ]

-- | Unit tests
votingContextSpec :: Script -> TestTree
votingContextSpec votingScript =
  let
    config = testConfigFromScript votingScript
    mkTest' = mkTest config
   in
    testGroup
      "Voting Context tests"
      [ mkTest' "OK case: Valid voting transaction" validVote ScriptSuccess
      , mkTest' "Fail case: transaction missing Authority token" missingGat3 ScriptFailure
      , mkTest' "Fail case: vote doesn't match the contents of the datum" mismatchingVote ScriptFailure
      , mkTest' "Fail case: transaction contains tx certs" hasTxCerts ScriptFailure
      , mkTest' "Fail case: transaction contains proposal procedures" hasPProcsVoting ScriptFailure
      , mkTest' "Fail case: transaction contains treasury donations" hasTrDonationsVoting ScriptFailure
      ]

-- | Unit tests
spendingContextSpec :: Script -> TestTree
spendingContextSpec votingScript =
  let
    config = testConfigFromScript votingScript
    mkTest' = mkTest config
   in
    testGroup
      "Spending Context tests"
      [ mkTest' "OK case: Valid spending transaction" validSpend ScriptSuccess
      , mkTest' "Fail case: transaction missing Authority token" missingGat3Spending ScriptFailure
      ]

-- -- * ScriptContexts for the test cases

validCert :: TestConfig -> ScriptContext
validCert config =
  let regDRep = TxCertRegDRep (DRepCredential (vsCredential config)) (Lovelace 5_000_000)
   in buildCertifying' $
        mconcat
          [ withCertifying regDRep
          , input (gat3Utxo config)
          , txCert regDRep
          ]

hasVotes :: TestConfig -> ScriptContext
hasVotes config =
  let regDRep = TxCertRegDRep (DRepCredential (vsCredential config)) (Lovelace 5_000_000)
   in buildCertifying' $
        mconcat
          [ withCertifying regDRep
          , input (gat3Utxo config)
          , vote
              (DRepVoter (DRepCredential (vsCredential config)))
              (GovernanceActionId (TxId "") 0)
              VoteYes
          , txCert regDRep
          ]

multipleTxCerts :: TestConfig -> ScriptContext
multipleTxCerts config =
  let regDRep = TxCertRegDRep (DRepCredential (vsCredential config)) 5_000_000
   in buildCertifying' $
        mconcat
          [ withCertifying regDRep
          , input (gat3Utxo config)
          , txCert regDRep
          , txCert (TxCertRegStaking (vsCredential config) (Just 5_000_000))
          ]

hasPProcs :: TestConfig -> ScriptContext
hasPProcs config =
  let regDRep = TxCertRegDRep (DRepCredential (vsCredential config)) 5_000_000
   in buildCertifying' $
        mconcat
          [ withCertifying regDRep
          , input (gat3Utxo config)
          , txCert regDRep
          , proposalProcedure (ProposalProcedure 5_000_000 (vsCredential config) InfoAction)
          ]

hasTrDonations :: TestConfig -> ScriptContext
hasTrDonations config =
  let regDRep = TxCertRegDRep (DRepCredential (vsCredential config)) (Lovelace 5_000_000)
   in buildCertifying' $
        mconcat
          [ withCertifying regDRep
          , input (gat3Utxo config)
          , txCert regDRep
          , treasuryDonation 5_000_000
          ]

validVote :: TestConfig -> ScriptContext
validVote config =
  let voter = DRepVoter (DRepCredential (vsCredential config))
   in buildVoting' $
        mconcat
          [ withVoting voter
          , input (gat3Utxo config)
          , vote
              voter
              (GovernanceActionId (TxId "") 0)
              VoteYes
          ]

missingGat3 :: TestConfig -> ScriptContext
missingGat3 config =
  let voter = DRepVoter (DRepCredential (vsCredential config))
   in buildVoting' $
        mconcat
          [ withVoting voter
          , vote
              voter
              (GovernanceActionId (TxId "") 0)
              VoteYes
          ]

mismatchingVote :: TestConfig -> ScriptContext
mismatchingVote config =
  let voter = DRepVoter (DRepCredential (vsCredential config))
   in buildVoting' $
        mconcat
          [ withVoting voter
          , input (gat3Utxo config)
          , vote
              voter
              (GovernanceActionId (TxId "") 0)
              VoteNo
          ]

hasTxCerts :: TestConfig -> ScriptContext
hasTxCerts config =
  let voter = DRepVoter (DRepCredential (vsCredential config))
   in buildVoting' $
        mconcat
          [ withVoting voter
          , input (gat3Utxo config)
          , txCert (TxCertRegStaking (vsCredential config) (Just 5_000_000))
          ]

hasPProcsVoting :: TestConfig -> ScriptContext
hasPProcsVoting config =
  let voter = DRepVoter (DRepCredential (vsCredential config))
   in buildVoting' $
        mconcat
          [ withVoting voter
          , input (gat3Utxo config)
          , proposalProcedure (ProposalProcedure 5_000_000 (vsCredential config) InfoAction)
          ]

hasTrDonationsVoting :: TestConfig -> ScriptContext
hasTrDonationsVoting config =
  let voter = DRepVoter (DRepCredential (vsCredential config))
   in buildVoting' $
        mconcat
          [ withVoting voter
          , input (gat3Utxo config)
          , treasuryDonation 5_000_000
          ]

validSpend :: TestConfig -> ScriptContext
validSpend config =
  buildSpending' $
    mconcat
      [ withSpendingUTXO (gat3Utxo config)
      , mint (Value.singleton gat3CurSym (TokenName "") (-1))
      , input (gat3Utxo config)
      ]

missingGat3Spending :: TestConfig -> ScriptContext
missingGat3Spending config =
  buildSpending' $
    mconcat
      [ withSpendingUTXO (gat3Utxo config)
      , input (gat3Utxo config)
      ]

-- | Proxy Datum, attached to the GAT v2 token's UTxO
votingDatum :: VotingDatum
votingDatum =
  VotingDatum
    { vdGovernanceActionId = GovernanceActionId (TxId "") 0
    , vdVote = VoteYes
    }

-- | GAT v2 currency symbol
gat3CurSym :: CurrencySymbol
gat3CurSym = CurrencySymbol "aabbcc"

-- | UTxO containing GAT v2 token
gat3Utxo :: TestConfig -> UTXO
gat3Utxo config =
  mconcat
    [ withValue (Value.singleton gat3CurSym (TokenName "") 1)
    , withHashDatum votingDatum
    , withCredential (vsCredential config)
    ]

-- -- * Test utilities

mkTest :: TestConfig -> String -> (TestConfig -> ScriptContext) -> ScriptResult -> TestTree
mkTest config testName toContext expectedResult =
  let script = ownScript config
      context = toContext config
      Script applied = uncheckedApplyDataToScript context $ uncheckedApplyDataToScript gat3CurSym script
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
