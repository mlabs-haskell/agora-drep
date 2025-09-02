{- | Voting Effect script unit tests

@since 1.0.0
-}
module Spec.Effect.Voting.Context (
  certifyingContextSpec,
  votingContextSpec,
  spendingContextSpec,
  validCert,
  validSpend,
  validVote,
) where

import Agora.Effect.Voting (VotingDatum (VotingDatum, vdGovernanceActionId, vdVote))
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

{- | Context for the test case

@since 1.0.0
-}
data TestConfigVoting = TestConfigVoting
  { vsCredential :: Credential
  , vsScript :: Script
  }

-- | @since 1.0.0
instance TestConfig TestConfigVoting where
  ownScript config = uncheckedApplyDataToScript gat3CurSym (vsScript config)

  testConfigFromScript votingScript =
    let votingScriptHash = scriptHash votingScript
     in TestConfigVoting
          { vsCredential = ScriptCredential votingScriptHash
          , vsScript = votingScript
          }

{- | Voting Effect script unit tests for certifying cases

@since 1.0.0
-}
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

{- | Voting Effect script unit tests for voting cases

@since 1.0.0
-}
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

{- | Voting Effect script unit tests for spending cases

@since 1.0.0
-}
spendingContextSpec :: Script -> TestTree
spendingContextSpec votingScript =
  let
    config = testConfigFromScript votingScript
    mkTest' = mkTest config
   in
    testGroup
      "Spending Context tests"
      [ mkTest' "OK case: Valid spending transaction" validSpend ScriptSuccess
      , mkTest' "Fail case: transaction not burning pGAT" missingGat3Burn ScriptFailure
      , mkTest' "Fail case: transaction missing Authority token" missingGat3Spending ScriptFailure
      ]

-- * ScriptContexts for the test cases

{- | Valid DRep registration certifying transaction (should pass)

@since 1.0.0
-}
validCert :: TestConfigVoting -> ScriptContext
validCert config =
  let regDRep = TxCertRegDRep (DRepCredential (vsCredential config)) (Lovelace 5_000_000)
   in buildCertifying' $
        mconcat
          [ withCertifying regDRep
          , input (gat3Utxo config)
          , txCert regDRep
          ]

{- | Certifying transaction also has votes (should fail)

@since 1.0.0
-}
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

{- | More than one certificates are included in the transaction (should fail)

@since 1.0.0
-}
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

{- | Proposal procedures are included in the certifying transaction (should fail)

@since 1.0.0
-}
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

{- | Treasury donations are included in the transaction (should fail)

@since 1.0.0
-}
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

{- | Valid voting transaction (should pass)

@since 1.0.0
-}
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

{- | Voting transaction is missing a valid Governance Authority Token
(should fail)

@since 1.0.0
-}
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

{- | DRep vote doesn't match the datum of the effect (should fail)

@since 1.0.0
-}
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

{- | Voting transaction includes certificates (should fail)

@since 1.0.0
-}
hasTxCerts :: TestConfigVoting -> ScriptContext
hasTxCerts config =
  let voter = DRepVoter (DRepCredential (vsCredential config))
   in buildVoting' $
        mconcat
          [ withVoting voter
          , input (gat3Utxo config)
          , txCert (TxCertRegStaking (vsCredential config) (Just 5_000_000))
          ]

{- | Voting transaction includes proposal procedures (should fail)

@since 1.0.0
-}
hasPProcsVoting :: TestConfigVoting -> ScriptContext
hasPProcsVoting config =
  let voter = DRepVoter (DRepCredential (vsCredential config))
   in buildVoting' $
        mconcat
          [ withVoting voter
          , input (gat3Utxo config)
          , proposalProcedure (ProposalProcedure 5_000_000 (vsCredential config) InfoAction)
          ]

{- | Voting transaction includes treasury donations (should fail)

@since 1.0.0
-}
hasTrDonationsVoting :: TestConfigVoting -> ScriptContext
hasTrDonationsVoting config =
  let voter = DRepVoter (DRepCredential (vsCredential config))
   in buildVoting' $
        mconcat
          [ withVoting voter
          , input (gat3Utxo config)
          , treasuryDonation 5_000_000
          ]

{- | Valid spend from the Voting Effect validator script address (should pass)

@since 1.0.0
-}
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

{- | Attempt to spend from Voting Effect Validator without burning a Governance
Authority Token (should fail)

@since 1.0.0
-}
missingGat3Burn :: TestConfigVoting -> ScriptContext
missingGat3Burn config =
  buildSpending' $
    mconcat
      [ withSpendingUTXO (gat3Utxo config)
      , input (gat3Utxo config)
      , vote
          (DRepVoter (DRepCredential (vsCredential config)))
          (GovernanceActionId (TxId "") 0)
          VoteYes
      ]

{- | Attempt to spend from Voting Effect Validator without spending a
Governance Authority Token (should fail)

@since 1.0.0
-}
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

{- | Proxy Datum, attached to the GAT v2 token's UTxO

@since 1.0.0
-}
votingDatum :: VotingDatum
votingDatum =
  VotingDatum
    { vdGovernanceActionId = GovernanceActionId (TxId "") 0
    , vdVote = VoteYes
    }

{- | GAT v2 currency symbol

@since 1.0.0
-}
gat3CurSym :: CurrencySymbol
gat3CurSym = CurrencySymbol "aabbcc"

{- | UTxO containing GAT v2 token

@since 1.0.0
-}
gat3Utxo :: TestConfigVoting -> UTXO
gat3Utxo config =
  mconcat
    [ withValue (Value.singleton gat3CurSym (TokenName "") 1)
    , withHashDatum votingDatum
    , withCredential (vsCredential config)
    ]
