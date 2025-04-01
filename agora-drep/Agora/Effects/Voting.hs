module Agora.Effects.Voting (votingEffectValidator, VotingDatum (..)) where

import Agora.AuthorityToken (singleAuthorityTokenBurned)
import Data.Kind (Type)
import GHC.Generics qualified as GHC
import Generics.SOP qualified as SOP
import Plutarch.LedgerApi.AssocMap qualified as AssocMap
import Plutarch.LedgerApi.V3 (
  PAddress (PAddress),
  PCredential (PScriptCredential),
  PCurrencySymbol,
  PDRepCredential (PDRepCredential),
  PDatum (PDatum),
  PGovernanceActionId,
  PMap (PMap),
  PMaybeData (PDNothing),
  POutputDatum (POutputDatumHash),
  PScriptContext (PScriptContext),
  PScriptInfo (PCertifyingScript, PSpendingScript, PVotingScript),
  PTxCert (PTxCertRegDRep),
  PTxInInfo (PTxInInfo),
  PTxInfo (PTxInfo),
  PTxOut (PTxOut),
  PVote,
  PVoter (PDRepVoter),
 )
import Plutarch.Monadic qualified as P
import Plutarch.Prelude (
  ClosedTerm,
  PAsData,
  PBool (PFalse, PTrue),
  PBuiltinList (PCons, PNil),
  PData,
  PEq ((#==)),
  PIsData,
  PListLike (phead, pnull),
  PTryFrom,
  PUnit (PUnit),
  PlutusType,
  S,
  Term,
  pcon,
  perror,
  pfilter,
  pfromData,
  pfstBuiltin,
  pif,
  plam,
  plength,
  pmap,
  pmatch,
  psndBuiltin,
  ptryFrom,
  tcont,
  unTermCont,
  (#),
  (#$),
  (#&&),
  (:-->),
 )
import Plutarch.Repr.Data (DeriveAsDataRec (DeriveAsDataRec))
import Plutarch.Trace (ptraceInfoIfFalse)
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

votingEffectValidator :: ClosedTerm (PAsData PCurrencySymbol :--> PAsData PScriptContext :--> PUnit)
votingEffectValidator = plam $ \authSymbol' ctx -> P.do
  PScriptContext txInfo _redeemer scriptInfo <- pmatch $ pfromData ctx

  PTxInfo inputs' _ _outputs _ mint' txCerts' _ _ _ _ datums' _ votes' pprocs' _ trDonations <- pmatch txInfo

  let inputs = pmap # plam pfromData # pfromData inputs'
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
            singleAuthorityTokenBurned authSymbol inputs mint
          PCertifyingScript _ txCert -> P.do
            let votes = pfromData votes'

            let hasNoVotes =
                  ptraceInfoIfFalse "Transaction must not contain any votes." $ AssocMap.pnull # votes

            let noProposalProcedures =
                  ptraceInfoIfFalse "Transaction must not contain any proposal procedures." $ pnull # pprocs

            let noTrDonations =
                  ptraceInfoIfFalse "Transaction must not contain any treasury donations." $ trDonations #== pcon PDNothing

            PCons _txCert txCerts' <- pmatch txCerts
            PNil <- pmatch txCerts'

            PTxCertRegDRep _ _ <- pmatch txCert

            hasNoVotes #&& noProposalProcedures #&& noTrDonations
          PVotingScript voter -> P.do
            PDRepVoter drepCred <- pmatch voter
            PDRepCredential cred <- pmatch drepCred
            PScriptCredential scriptHash <- pmatch cred

            let noProposalProcedures =
                  ptraceInfoIfFalse "Transaction must not contain any proposal procedures." $ pnull # pprocs

            let noTrDonations =
                  ptraceInfoIfFalse "Transaction must not contain any treasury donations." $ trDonations #== pcon PDNothing

            -- Filtering inputs to later verify that only one script input exists in the transaction
            let scriptInputs =
                  pfilter
                    # plam
                      ( \input -> pmatch input $ \case
                          PTxInInfo _ resolved -> P.do
                            PTxOut addr _ _ _ <- pmatch resolved
                            PAddress cred _ <- pmatch addr
                            pmatch cred $ \case
                              PScriptCredential _ -> pcon PTrue
                              _ -> pcon PFalse
                      )
                    # inputs

            POutputDatumHash datumHash <-
              pmatch $
                phead
                  #$ pmap
                  # plam
                    ( \input -> pmatch input $ \case
                        PTxInInfo _ resolved -> P.do
                          PTxOut _ _ datum _ <- pmatch resolved
                          datum
                    )
                  #$ pfilter
                  # plam
                    ( \input -> pmatch input $ \case
                        PTxInInfo _ resolved -> P.do
                          PTxOut addr _ _ _ <- pmatch resolved
                          PAddress cred _ <- pmatch addr
                          PScriptCredential scriptHash' <- pmatch cred
                          scriptHash #== scriptHash'
                    )
                  # scriptInputs

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

            let datumCheck =
                  (datumHash #== datumHash')
                    #&& (govActionId #== govActionId')
                    #&& (vote #== vote')

            -- Spending condition 6: Transaction does not include script inputs other than own input.
            let singleScriptInput =
                  ptraceInfoIfFalse "Transaction must not include script inputs other than own input." $
                    (plength # scriptInputs) #== 1

            let hasNoCerts =
                  ptraceInfoIfFalse "Transaction must contain exactly 1 transaction certificate." $
                    (plength # txCerts) #== 0

            foldr1
              (#&&)
              [ datumCheck
              , singleScriptInput
              , hasNoCerts
              , noProposalProcedures
              , noTrDonations
              ]
          _ -> pcon PFalse

  pif valid (pcon PUnit) perror
