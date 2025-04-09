module Agora.Effect.Voting (votingEffectScript, VotingDatum (..)) where

import Agora.AuthorityToken (singleAuthorityTokenBurned)
import Data.Kind (Type)
import GHC.Generics qualified as GHC
import Generics.SOP qualified as SOP
import Plutarch.LedgerApi.V3 (
  PAddress (PAddress),
  PCredential (PScriptCredential),
  PCurrencySymbol,
  PDRepCredential (PDRepCredential),
  PDatum (PDatum),
  PDatumHash,
  PGovernanceActionId,
  PMap (PMap),
  PMaybeData (PDNothing),
  POutputDatum (POutputDatumHash),
  PScriptContext (PScriptContext),
  PScriptHash,
  PScriptInfo (PCertifyingScript, PSpendingScript, PVotingScript),
  PTxCert (PTxCertRegDRep),
  PTxInInfo (PTxInInfo),
  PTxInfo (PTxInfo),
  PTxOut (PTxOut),
  PVote,
  PVoter (PDRepVoter),
 )
import Plutarch.Maybe (pjust)
import Plutarch.Monadic qualified as P
import Plutarch.Prelude (
  ClosedTerm,
  PAsData,
  PBool (PFalse, PTrue),
  PBuiltinList (PCons, PNil),
  PData,
  PEq ((#==)),
  PIsData,
  PMaybe (PJust, PNothing),
  PTryFrom,
  PUnit (PUnit),
  PlutusType,
  S,
  Term,
  pcon,
  pconstant,
  perror,
  pfromData,
  pfstBuiltin,
  pif,
  plam,
  pmatch,
  precList,
  psndBuiltin,
  ptryFrom,
  tcont,
  unTermCont,
  (#),
  (#&&),
  (:-->),
 )
import Plutarch.Repr.Data (DeriveAsDataRec (DeriveAsDataRec))
import PlutusLedgerApi.V3 (
  FromData (fromBuiltinData),
  GovernanceActionId,
  Vote,
  toBuiltin,
  toData,
 )
import PlutusTx qualified
import PlutusTx.Builtins (chooseData, unsafeDataAsList)

{- | Haskell-level datum for the Voting Effect Validator script.

 @since 0.1.0
-}
data VotingDatum = VotingDatum
  { vdGovernanceActionId :: GovernanceActionId
  , vdVote :: Vote
  }

instance PlutusTx.FromData VotingDatum where
  {-# INLINEABLE fromBuiltinData #-}
  fromBuiltinData d =
    chooseData
      d
      (const Nothing)
      (const Nothing)
      ( \d' ->
          case unsafeDataAsList d' of
            [governanceActionId, vote] ->
              VotingDatum
                <$> fromBuiltinData governanceActionId
                <*> fromBuiltinData vote
            _ -> Nothing
      )
      (const Nothing)
      (const Nothing)
      d

instance PlutusTx.ToData VotingDatum where
  {-# INLINEABLE toBuiltinData #-}
  toBuiltinData (VotingDatum governanceActionId vote) =
    toBuiltin $ PlutusTx.List [toData governanceActionId, toData vote]

type PVotingDatum :: S -> Type
data PVotingDatum s = PVotingDatum
  { governanceActionId :: Term s (PAsData PGovernanceActionId)
  , vote :: Term s (PAsData PVote)
  }
  deriving stock (GHC.Generic)
  deriving anyclass (SOP.Generic, PEq, PIsData)
  deriving (PlutusType) via (DeriveAsDataRec PVotingDatum)

instance PTryFrom PData (PAsData PVotingDatum)

votingEffectScript :: ClosedTerm (PAsData PCurrencySymbol :--> PAsData PScriptContext :--> PUnit)
votingEffectScript = plam $ \authSymbol' ctx -> P.do
  PScriptContext txInfo _redeemer scriptInfo <- pmatch $ pfromData ctx

  PTxInfo inputs _ _outputs _ mint' txCerts' _ _ _ _ datums' _ votes' pprocs' _ trDonations <- pmatch txInfo

  let txCerts = pfromData txCerts'
  let votes = pfromData votes'
  let pprocs = pfromData pprocs'
  let datums = pfromData datums'

  let valid = pmatch scriptInfo $
        \case
          PSpendingScript _txOutRef _datum -> P.do
            let mint = pfromData mint'

            let authSymbol = pfromData authSymbol'
            -- Spending Condition 1: Transaction burns one GAT
            -- Spending Condition 2: Spent UTxO contains GAT

            -- Spending condition 6: Transaction does not include script inputs other than own input.
            -- If there is only one script input and we are running that means that we are that input
            -- so no need to check explicitly
            PJust ownScriptHash <-
              pmatch $
                precList
                  ( \self input rest -> pmatch (pfromData input) $ \case
                      PTxInInfo _ resolved -> P.do
                        PTxOut addr _ _ _ <- pmatch resolved
                        PAddress cred _ <- pmatch addr
                        pmatch cred $ \case
                          PScriptCredential hash -> pmatch (self # rest) $ \case
                            PNothing -> pjust # hash
                            PJust _ -> perror
                          _ -> self # rest
                  )
                  (const (pconstant @(PMaybe (PAsData PScriptHash)) Nothing))
                  # pfromData inputs

            PMap voteList <- pmatch votes
            PCons votePair voteList' <- pmatch voteList
            PNil <- pmatch voteList'

            PDRepVoter voter <- pmatch . pfromData $ pfstBuiltin # votePair
            PDRepCredential cred <- pmatch voter
            PScriptCredential scriptHash <- pmatch cred

            singleAuthorityTokenBurned authSymbol (pfromData inputs) mint
              #&& (scriptHash #== ownScriptHash)
          PCertifyingScript _ txCert -> P.do
            let votes = pfromData votes'

            -- Transaction must not contain any votes.
            PMap votesList <- pmatch votes
            PNil <- pmatch votesList

            -- Transaction must not contain any proposal procedures.
            PNil <- pmatch pprocs

            -- Transaction must not contain any treasury donations.
            PDNothing <- pmatch trDonations

            -- Transaction must contain exactly 1 transaction certificate.
            PCons _txCert txCerts' <- pmatch txCerts
            PNil <- pmatch txCerts'

            -- Transaction certificate must be a DRep registration
            PTxCertRegDRep _ _ <- pmatch txCert

            pcon PTrue
          PVotingScript _voter -> P.do
            -- Transaction must not contain any proposal procedures.
            PNil <- pmatch pprocs

            -- Transaction must not contain any treasury donations.
            PDNothing <- pmatch trDonations

            -- The spending script ensures that there is only one script input
            -- The spending input will contain a datum, and we don't allow any
            -- more datums in the transaction
            PJust datumHash <-
              pmatch $
                precList
                  ( \self input rest -> pmatch (pfromData input) $ \case
                      PTxInInfo _ resolved -> P.do
                        PTxOut addr _ datum _ <- pmatch resolved
                        POutputDatumHash datumHash <- pmatch datum
                        PAddress cred _ <- pmatch addr
                        pmatch cred $ \case
                          PScriptCredential _ -> pmatch (self # rest) $ \case
                            PNothing -> pjust # datumHash
                            PJust _ -> perror
                          _ -> self # rest
                  )
                  (const (pconstant @(PMaybe (PAsData PDatumHash)) Nothing))
                  # pfromData inputs

            -- There must be exactly 1 vote. As we're in a Voting context, this is guaranteed
            -- to be the vote with the current script as DRep voter
            PMap voteList <- pmatch votes
            PCons votePair voteList' <- pmatch voteList
            PNil <- pmatch voteList'
            let ownVotes = pfromData $ psndBuiltin # votePair

            PMap ownVoteList <- pmatch ownVotes
            PCons ownVote ownVoteList' <- pmatch ownVoteList
            PNil <- pmatch ownVoteList'

            let govActionId = pfstBuiltin # ownVote
            let vote = psndBuiltin # ownVote

            PMap datumList <- pmatch datums
            PCons datum datumList' <- pmatch datumList
            PNil <- pmatch datumList'

            let datumHash' = pfstBuiltin # datum
            PDatum rawDatum <- pmatch . pfromData $ psndBuiltin # datum

            let rawVotingDatum = unTermCont $ fst <$> tcont (ptryFrom @(PAsData PVotingDatum) rawDatum)
            PVotingDatum govActionId' vote' <- pmatch $ pfromData rawVotingDatum

            -- Transaction must not contain transaction certificate.
            PNil <- pmatch txCerts

            -- Governance Action Id and Vote must match the values in the datum
            (datumHash #== datumHash')
              #&& (govActionId #== govActionId')
              #&& (vote #== vote')
          _ -> pcon PFalse

  pif valid (pcon PUnit) perror
