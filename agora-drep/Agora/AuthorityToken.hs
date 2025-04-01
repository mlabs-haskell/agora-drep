-- | This module was copied from the Liqwid-Labs/agora repo, and updated to work with Plutus V3 and Plutarch 1.10.1
module Agora.AuthorityToken (singleAuthorityTokenBurned, authorityTokensValidIn) where

import Agora.Utils (psymbolValueOf)
import Plutarch.Builtin.Bool (PBool)
import Plutarch.LedgerApi.AssocMap qualified as AssocMap
import Plutarch.LedgerApi.V3 (
  AmountGuarantees,
  KeyGuarantees,
  PAddress (PAddress),
  PCredential (PPubKeyCredential, PScriptCredential),
  PCurrencySymbol,
  PTxInInfo (PTxInInfo),
  PTxOut (PTxOut),
  PValue (PValue),
 )
import Plutarch.Monadic qualified as P
import Plutarch.Prelude (
  PAsData,
  PBool (PTrue),
  PBuiltinList,
  PEq ((#==)),
  PInteger,
  PMaybe (PJust, PNothing),
  S,
  Term,
  pcon,
  pconstant,
  pfoldr,
  pfromData,
  phoistAcyclic,
  pif,
  plam,
  pmatch,
  ptraceInfoIfFalse,
  (#),
  (#&&),
  (:-->),
 )

{- | Check that all GATs are valid in a particular TxOut.

  WARNING: As of version 1.0.0, this has been weakened in order to be
  compatible with RATs. The token name is no longer checked, meaning that a
  GAT can escape from its effect script, if the effect script is vulnerable.
  In order to prevent this, all effect scripts should be implemented carefully,
  and ideally use the trusted effect base. See also 'Agora.Effect'.

  (before 1.0.0) How this is checked: an AuthorityToken should never leave
  the Effect it was initially sent to, so we simply check that
  the script address the token resides in matches the TokenName.
  Since the TokenName was tagged upon mint with the Effect script
  it was sent to, this is enough to prove validity.
  In other words, check that all assets of a particular currency symbol
  are tagged with a TokenName that matches where they live.

  @since 1.0.0
-}
authorityTokensValidIn :: forall (s :: S). Term s (PCurrencySymbol :--> PTxOut :--> PBool)
authorityTokensValidIn = phoistAcyclic $
  plam $ \authorityTokenSym txOut -> P.do
    PTxOut address value _ _ <- pmatch txOut
    PAddress credential _ <- pmatch address
    PValue assetMap <- pmatch $ pfromData value
    pmatch (AssocMap.plookup # authorityTokenSym # assetMap) $ \case
      PJust _tokenMap ->
        -- TODO: This check only needs to happen for outputs, not inputs (performance)
        pmatch credential $ \case
          PPubKeyCredential _ ->
            -- GATs should only be sent to Effect validators
            ptraceInfoIfFalse "authorityTokensValidIn: GAT incorrectly lives at PubKey" $ pconstant False
          PScriptCredential _ ->
            -- NOTE: We no longer can perform a check on `TokenName` content here.
            -- Instead, the auth check system uses `TokenName`s, but it cannot
            -- check for GATs incorrectly escaping scripts. The effect scripts
            -- need to be written very carefully in order to disallow this.
            pcon PTrue
      PNothing ->
        -- No GATs exist at this output!
        pcon PTrue

{- | Assert that a single authority token has been burned.

  @since 0.2.0
-}
singleAuthorityTokenBurned ::
  forall (keys :: KeyGuarantees) (amounts :: AmountGuarantees) (s :: S).
  Term s PCurrencySymbol ->
  Term s (PBuiltinList (PAsData PTxInInfo)) ->
  Term s (PValue keys amounts) ->
  Term s PBool
singleAuthorityTokenBurned gatCs inputs mint = P.do
  let gatAmountMinted :: Term _ PInteger
      gatAmountMinted = psymbolValueOf # gatCs # mint

  let inputsWithGAT =
        pfoldr
          # plam
            ( \input v -> pmatch (pfromData input) $ \case
                PTxInInfo _txOutRef resolved ->
                  pif
                    (authorityTokensValidIn # gatCs # resolved)
                    ( P.do
                        PTxOut _ value _ _ <- pmatch resolved

                        v + (psymbolValueOf # gatCs # pfromData value)
                    )
                    (P.fail "While counting GATs at inputs: all GATs must be valid")
            )
          # 0
          # inputs

  foldr1
    (#&&)
    [ ptraceInfoIfFalse "singleAuthorityTokenBurned: Must burn exactly 1 GAT" $
        gatAmountMinted #== -1
    , ptraceInfoIfFalse "Only one GAT must exist at the inputs" $
        inputsWithGAT #== 1
    ]
