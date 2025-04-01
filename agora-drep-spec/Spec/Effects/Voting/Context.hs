module Spec.Effects.Voting.Context (
  certifyingContextSpec,
  votingContextSpec,
  spendingContextSpec,
) where

import Agora.Effects.Voting (VotingDatum (VotingDatum, vdGovernanceActionId, vdVote))
import Plutarch.LedgerApi.V3 (scriptHash)
import Plutarch.Script (Script)
import Plutarch.Test.Program (ScriptResult (ScriptFailure, ScriptSuccess))
import Plutus.ContextBuilder (UTXO, buildCertifying', buildSpending', buildVoting', input, mint, proposalProcedure, treasuryDonation, txCert, vote, withCertifying, withCredential, withHashDatum, withSpendingUTXO, withValue, withVoting)
import PlutusLedgerApi.V1.Value qualified as Value
import PlutusLedgerApi.V3 (
  Credential (ScriptCredential),
  CurrencySymbol (CurrencySymbol),
  DRepCredential (DRepCredential),
  GovernanceAction (InfoAction),
  GovernanceActionId (GovernanceActionId),
  Lovelace (Lovelace),
  ProposalProcedure (ProposalProcedure),
  ScriptContext,
  TokenName (TokenName),
  TxId (TxId),
  Vote (VoteNo, VoteYes),
 )
import PlutusLedgerApi.V3.Contexts (TxCert (TxCertRegDRep, TxCertRegStaking), Voter (DRepVoter))
import Spec.Utils (TestConfig (ownScript), mkTest, testConfigFromScript, uncheckedApplyDataToScript)
import Test.Tasty (TestTree, testGroup)

data TestConfigVoting = TestConfigVoting
  { vsCredential :: Credential
  , vsScript :: Script
  }

instance TestConfig TestConfigVoting where
  ownScript config = uncheckedApplyDataToScript gat3CurSym (vsScript config)

  testConfigFromScript votingScript =
    let votingScriptHash = scriptHash votingScript
     in TestConfigVoting
          { vsCredential = ScriptCredential votingScriptHash
          , vsScript = votingScript
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

validCert :: TestConfigVoting -> ScriptContext
validCert config =
  let regDRep = TxCertRegDRep (DRepCredential (vsCredential config)) (Lovelace 5_000_000)
   in buildCertifying' $
        mconcat
          [ withCertifying regDRep
          , input (gat3Utxo config)
          , txCert regDRep
          ]

hasVotes :: TestConfigVoting -> ScriptContext
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

multipleTxCerts :: TestConfigVoting -> ScriptContext
multipleTxCerts config =
  let regDRep = TxCertRegDRep (DRepCredential (vsCredential config)) 5_000_000
   in buildCertifying' $
        mconcat
          [ withCertifying regDRep
          , input (gat3Utxo config)
          , txCert regDRep
          , txCert (TxCertRegStaking (vsCredential config) (Just 5_000_000))
          ]

hasPProcs :: TestConfigVoting -> ScriptContext
hasPProcs config =
  let regDRep = TxCertRegDRep (DRepCredential (vsCredential config)) 5_000_000
   in buildCertifying' $
        mconcat
          [ withCertifying regDRep
          , input (gat3Utxo config)
          , txCert regDRep
          , proposalProcedure (ProposalProcedure 5_000_000 (vsCredential config) InfoAction)
          ]

hasTrDonations :: TestConfigVoting -> ScriptContext
hasTrDonations config =
  let regDRep = TxCertRegDRep (DRepCredential (vsCredential config)) (Lovelace 5_000_000)
   in buildCertifying' $
        mconcat
          [ withCertifying regDRep
          , input (gat3Utxo config)
          , txCert regDRep
          , treasuryDonation 5_000_000
          ]

validVote :: TestConfigVoting -> ScriptContext
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

missingGat3 :: TestConfigVoting -> ScriptContext
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

mismatchingVote :: TestConfigVoting -> ScriptContext
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

hasTxCerts :: TestConfigVoting -> ScriptContext
hasTxCerts config =
  let voter = DRepVoter (DRepCredential (vsCredential config))
   in buildVoting' $
        mconcat
          [ withVoting voter
          , input (gat3Utxo config)
          , txCert (TxCertRegStaking (vsCredential config) (Just 5_000_000))
          ]

hasPProcsVoting :: TestConfigVoting -> ScriptContext
hasPProcsVoting config =
  let voter = DRepVoter (DRepCredential (vsCredential config))
   in buildVoting' $
        mconcat
          [ withVoting voter
          , input (gat3Utxo config)
          , proposalProcedure (ProposalProcedure 5_000_000 (vsCredential config) InfoAction)
          ]

hasTrDonationsVoting :: TestConfigVoting -> ScriptContext
hasTrDonationsVoting config =
  let voter = DRepVoter (DRepCredential (vsCredential config))
   in buildVoting' $
        mconcat
          [ withVoting voter
          , input (gat3Utxo config)
          , treasuryDonation 5_000_000
          ]

validSpend :: TestConfigVoting -> ScriptContext
validSpend config =
  buildSpending' $
    mconcat
      [ withSpendingUTXO (gat3Utxo config)
      , mint (Value.singleton gat3CurSym (TokenName "") (-1))
      , input (gat3Utxo config)
      , vote
          (DRepVoter (DRepCredential (vsCredential config)))
          (GovernanceActionId (TxId "") 0)
          VoteYes
      ]

missingGat3Spending :: TestConfigVoting -> ScriptContext
missingGat3Spending config =
  buildSpending' $
    mconcat
      [ withSpendingUTXO (gat3Utxo config)
      , input (gat3Utxo config)
      , vote
          (DRepVoter (DRepCredential (vsCredential config)))
          (GovernanceActionId (TxId "") 0)
          VoteYes
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
gat3Utxo :: TestConfigVoting -> UTXO
gat3Utxo config =
  mconcat
    [ withValue (Value.singleton gat3CurSym (TokenName "") 1)
    , withHashDatum votingDatum
    , withCredential (vsCredential config)
    ]
