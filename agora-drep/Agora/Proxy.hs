module Agora.Proxy (proxyScript, PProxyDatum (..), ProxyDatum (..)) where

import Agora.AuthorityToken (singleAuthorityTokenBurned)
import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.Internal.Term (Term)
import Plutarch.LedgerApi.V2 (PDatumHash)
import Plutarch.LedgerApi.V3 (
    PCurrencySymbol,
    PScriptContext (PScriptContext),
    PScriptHash,
    PScriptInfo (PMintingScript, PSpendingScript),
    PTxInfo (PTxInfo),
 )
import Plutarch.Monadic qualified as P
import Plutarch.Prelude (
    ClosedTerm,
    DerivePlutusType (DPTStrat),
    PAsData,
    PBool (PFalse),
    PDataFields,
    PDataRecord,
    PEq,
    PLabeledType ((:=)),
    PShow,
    PUnit (PUnit),
    PlutusType,
    PlutusTypeData,
    S,
    pcon,
    perror,
    pfromData,
    pif,
    plam,
    pmap,
    pmatch,
    ptraceInfo,
    ptraceInfoIfFalse,
    (#),
    -- (#&&),
    (:-->),
 )
import PlutusLedgerApi.V3 (DatumHash, ScriptHash)

{- | Haskell-level datum for the Proxy Validator script.

    @since 0.1.0
-}
data ProxyDatum = ProxyDatum
    { receiverScript :: ScriptHash
    , datumHash :: DatumHash
    }
    deriving stock (Show, Generic)
    deriving anyclass (SOP.Generic)

newtype PProxyDatum (s :: S)
    = PProxyDatum
        ( Term
            s
            ( PDataRecord
                '[ "receiverScript" ':= PScriptHash
                 , "datumHash" ':= PDatumHash
                 ]
            )
        )
    deriving stock (Generic)
    deriving anyclass
        ( PlutusType
        , PDataFields
        , PEq
        , PShow
        )

instance DerivePlutusType PProxyDatum where
    type DPTStrat _ = PlutusTypeData

proxyScript :: ClosedTerm (PAsData PCurrencySymbol :--> PAsData PScriptContext :--> PUnit)
proxyScript = plam $ \authSymbol ctx -> P.do
    PScriptContext txInfo _redeemer scriptInfo <- pmatch $ pfromData ctx
    let valid =
            pmatch scriptInfo $ \case
                PSpendingScript _txOutRef _datum -> P.do
                    PTxInfo inputs _ _ _ mint _ _ _ _ _ _ _ _ _ _ _ <- pmatch txInfo

                    let singleAuthTokenBurned =
                            singleAuthorityTokenBurned
                                (pfromData authSymbol)
                                (pmap # plam pfromData # pfromData inputs)
                                (pfromData mint)

                    ptraceInfoIfFalse "[ProxyValidator spending]" singleAuthTokenBurned
                -- foldr1 (#&&) [singleAuthTokenBurned]

                PMintingScript _ -> pcon PFalse
                _ -> pcon PFalse

    pif valid (pcon PUnit) (ptraceInfo "[ProxyValidator] Validation failed" perror)
