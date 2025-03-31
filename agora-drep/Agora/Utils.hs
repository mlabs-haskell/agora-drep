module Agora.Utils (
  psymbolValueOf,
  pcountIf,
  pcurrencySymbolToScriptHash,
  pscriptHashToCurrencySymbol,
) where

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

  @since 1.1.0
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
pcountIf :: (PIsListLike list a) => Term s ((a :--> PBool) :--> list a :--> PInteger)
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

pcurrencySymbolToScriptHash :: Term s (PAsData PCurrencySymbol) -> Term s (PAsData PScriptHash)
pcurrencySymbolToScriptHash = punsafeCoerce

pscriptHashToCurrencySymbol :: Term s (PAsData PScriptHash) -> Term s (PAsData PCurrencySymbol)
pscriptHashToCurrencySymbol = punsafeCoerce
