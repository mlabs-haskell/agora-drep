module Agora.Effects.Voting (votingEffectValidator, VotingDatum (..)) where

import Agora.AuthorityToken (singleAuthorityTokenBurned)
import Data.Kind (Type)
import GHC.Generics qualified as GHC
import Generics.SOP qualified as SOP
import Plutarch.LedgerApi.AssocMap qualified as AssocMap
import Plutarch.LedgerApi.V3 (PCurrencySymbol, PGovernanceActionId, PMaybeData (PDNothing), PScriptContext (PScriptContext), PScriptInfo (PCertifyingScript), PTxInfo (PTxInfo), PVote)
import Plutarch.Monadic qualified as P
import Plutarch.Prelude (
  ClosedTerm,
  PAsData,
  PBool (PFalse),
  PData,
  PEq ((#==)),
  PIsData,
  PListLike (pnull),
  PTryFrom,
  PUnit (PUnit),
  PlutusType,
  S,
  Term,
  pcon,
  perror,
  pfromData,
  pif,
  plam,
  plength,
  pmap,
  pmatch,
  (#),
  (#&&),
  (:-->),
 )
import Plutarch.Repr.Data (DeriveAsDataRec (DeriveAsDataRec))
import Plutarch.Trace (ptraceInfoIfFalse)
import PlutusLedgerApi.V3 (FromData (fromBuiltinData), GovernanceActionId, Vote, toBuiltin, toData)
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

  PTxInfo inputs' _ _outputs _ mint' txCerts' _ _ _ _ _ _ votes' pprocs' _ trDonations <- pmatch txInfo

  let mint = pfromData mint'
  let inputs = pmap # plam pfromData # pfromData inputs'
  let txCerts = pfromData txCerts'
  let pprocs = pfromData pprocs'

  let authSymbol = pfromData authSymbol'
  -- Spending Condition 1: Transaction burns one GAT (symbol is known from script parameter)
  -- Spending Condition 2: Spent UTxO contains GAT
  let singleAuthTokenBurned = singleAuthorityTokenBurned authSymbol inputs mint

  let noProposalProcedures =
        ptraceInfoIfFalse "Transaction must not contain any proposal procedures." $ pnull # pprocs

  let noTrDonations =
        ptraceInfoIfFalse "Transaction must not contain any treasury donations." $ trDonations #== pcon PDNothing

  let purposeValidations = pmatch scriptInfo $
        \case
          PCertifyingScript _ _ -> P.do
            let votes = pfromData votes'

            let hasNoVotes =
                  ptraceInfoIfFalse "Transaction must not contain any votes." $ AssocMap.pnull # votes

            let has1Cert =
                  ptraceInfoIfFalse "Transaction must contain exactly 1 transaction certificate." $
                    (plength # txCerts) #== 1

            hasNoVotes #&& has1Cert
          _ -> pcon PFalse

  let valid =
        foldr1
          (#&&)
          [ singleAuthTokenBurned
          , noProposalProcedures
          , noTrDonations
          , purposeValidations
          ]

  pif valid (pcon PUnit) perror
