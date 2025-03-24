module Agora.Proxy (proxyScript, PProxyDatum (..), ProxyDatum (..)) where

import Agora.AuthorityToken (singleAuthorityTokenBurned)

-- (#&&),

import Data.Kind (Type)
import GHC.Generics qualified as GHC
import Generics.SOP qualified as SOP
import Plutarch.Internal.Term (Term)
import Plutarch.LedgerApi.Utils (pmaybeDataToMaybe, pmaybeToMaybeData)
import Plutarch.LedgerApi.V3 (
  PAddress (PAddress),
  PCredential (PScriptCredential),
  PCurrencySymbol,
  PDatum (PDatum),
  PDatumHash,
  POutputDatum (POutputDatumHash),
  PScriptContext (PScriptContext),
  PScriptHash,
  PScriptInfo (PMintingScript, PSpendingScript),
  PTxInfo (PTxInfo),
  PTxOut (PTxOut),
 )
import Plutarch.Monadic qualified as P
import Plutarch.Prelude (
  ClosedTerm,
  PAsData,
  PBool (PFalse),
  PData,
  PEq ((#==)),
  PInteger,
  PIsData,
  PMaybe (PJust, PNothing),
  PTryFrom,
  PUnit (PUnit),
  PlutusType,
  S,
  pcon,
  perror,
  pfoldr,
  pfromData,
  pif,
  plam,
  pmap,
  pmatch,
  ptraceInfo,
  ptraceInfoIfFalse,
  ptryFrom,
  tcont,
  unTermCont,
  (#),
  (#&&),
  (:-->),
 )
import Plutarch.Repr.Data (DeriveAsDataRec (DeriveAsDataRec))
import PlutusLedgerApi.V3 (DatumHash, ScriptHash, ToData (toBuiltinData))

{- | Haskell-level datum for the Proxy Validator script.

   @since 0.1.0
-}
data ProxyDatum = ProxyDatum
  { receiverScript :: ScriptHash
  , datumHash :: DatumHash
  }
  deriving stock (Show, GHC.Generic)
  deriving anyclass (SOP.Generic)

instance ToData ProxyDatum where
  toBuiltinData (ProxyDatum script datum) = toBuiltinData [toBuiltinData script, toBuiltinData datum]

type PProxyDatum :: S -> Type
data PProxyDatum s = PProxyDatum
  { receiverScript :: Term s (PAsData PScriptHash)
  , datumHash :: Term s (PAsData PDatumHash)
  }
  deriving stock (GHC.Generic)
  deriving anyclass (SOP.Generic, PEq, PIsData)
  deriving (PlutusType) via (DeriveAsDataRec PProxyDatum)

instance PTryFrom PData (PAsData PProxyDatum)

proxyScript :: ClosedTerm (PAsData PCurrencySymbol :--> PAsData PScriptContext :--> PUnit)
proxyScript = plam $ \authSymbol ctx -> P.do
  PScriptContext txInfo _redeemer scriptInfo <- pmatch $ pfromData ctx
  let valid =
        pmatch scriptInfo $ \case
          PSpendingScript _txOutRef mayDatum -> P.do
            PTxInfo inputs _ outputs _ mint _ _ _ _ _ _ _ _ _ _ _ <- pmatch txInfo

            let singleAuthTokenBurned =
                  singleAuthorityTokenBurned
                    (pfromData authSymbol)
                    (pmap # plam pfromData # pfromData inputs)
                    (pfromData mint)

            PJust datum <- pmatch $ pmaybeDataToMaybe # mayDatum
            PDatum rawDatum <- pmatch datum

            let rawProxyDatum = unTermCont $ fst <$> tcont (ptryFrom @(PAsData PProxyDatum) rawDatum)
            PProxyDatum receiverScript datumHash <- pmatch $ pfromData rawProxyDatum

            let receiverScriptAddr =
                  pcon $
                    PAddress
                      (pcon $ PScriptCredential receiverScript)
                      (pmaybeToMaybeData # pcon PNothing)

            let outputs' = pmap # plam pfromData # pfromData outputs

            let outputsAtReceiver =
                  pfoldr
                    # plam
                      ( \output v -> P.do
                          PTxOut address _ outputDatum _ <- pmatch output
                          pif
                            (address #== receiverScriptAddr)
                            ( P.do
                                POutputDatumHash datumHash' <- pmatch outputDatum

                                pif
                                  (datumHash' #== datumHash)
                                  (v + 1)
                                  (P.fail "While counting outputs at receiver address: datum hash invalid.")
                            )
                            v
                      )
                    # (0 :: Term _ PInteger)
                    # outputs'

            let singleOutputWithDatum = ptraceInfoIfFalse "Exactly one output at the receiver script address should exist with datum hash defined." $ outputsAtReceiver #== 1

            ptraceInfoIfFalse "[ProxyValidator spending]" $
              foldr1
                (#&&)
                [ singleAuthTokenBurned
                , singleOutputWithDatum
                ]
          PMintingScript _ -> pcon PFalse
          _ -> pcon PFalse

  pif valid (pcon PUnit) (ptraceInfo "[ProxyValidator] Validation failed" perror)
