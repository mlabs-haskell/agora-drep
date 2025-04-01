-- | @since WIP
module Agora.Utils (
  psymbolValueOf,
  pcountIf,
  pcurrencySymbolToScriptHash,
  pscriptHashToCurrencySymbol,
) where

import Data.Kind (Type)
import Plutarch.Internal.Term (punsafeCoerce)
import Plutarch.LedgerApi.AssocMap qualified as AssocMap
import Plutarch.LedgerApi.V3 (AmountGuarantees, KeyGuarantees, PCurrencySymbol, PMap (PMap), PScriptHash, PValue (PValue))
import Plutarch.Monadic qualified as P
import Plutarch.Prelude (
  PAsData,
  PBool,
  PInteger,
  PIsListLike,
  PMaybe (PJust, PNothing),
  S,
  Term,
  pfoldr,
  pfromData,
  phoistAcyclic,
  pif,
  plam,
  pmatch,
  precList,
  psndBuiltin,
  (#),
  (:-->),
 )

{- | Get the sum of all values belonging to a particular CurrencySymbol.

  @since WIP
-}
psymbolValueOf ::
  forall (keys :: KeyGuarantees) (amounts :: AmountGuarantees) (s :: S).
  Term s (PCurrencySymbol :--> PValue keys amounts :--> PInteger)
psymbolValueOf =
  phoistAcyclic $
    plam $ \sym value' -> P.do
      PValue value <- pmatch value'
      pmatch (AssocMap.plookup # sym # value) $ \case
        PJust m' ->
          pmatch m' $ \case
            PMap m -> pfoldr # plam (\x v -> pfromData (psndBuiltin # x) + v) # 0 # m
        PNothing -> 0

{- | Fused filter and length

@since WIP
-}
pcountIf ::
  forall (list :: (S -> Type) -> (S -> Type)) (a :: S -> Type) (s :: S).
  (PIsListLike list a) =>
  Term s ((a :--> PBool) :--> list a :--> PInteger)
pcountIf =
  phoistAcyclic $
    plam $ \predicate ->
      precList
        ( \self x xs ->
            pif
              (predicate # x)
              (1 + (self # xs))
              (self # xs)
        )
        (const 0)

-- | @since WIP
pcurrencySymbolToScriptHash ::
  forall (s :: S).
  Term s (PAsData PCurrencySymbol) ->
  Term s (PAsData PScriptHash)
pcurrencySymbolToScriptHash = punsafeCoerce

-- | @since WIP
pscriptHashToCurrencySymbol ::
  forall (s :: S).
  Term s (PAsData PScriptHash) ->
  Term s (PAsData PCurrencySymbol)
pscriptHashToCurrencySymbol = punsafeCoerce
