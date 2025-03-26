module Agora.Proxy (proxyScript, PProxyDatum (..), ProxyDatum (..)) where

import Agora.AuthorityToken (singleAuthorityTokenBurned)

import Agora.Utils (psymbolValueOf)
import Data.Kind (Type)
import GHC.Generics qualified as GHC
import Generics.SOP qualified as SOP
import Plutarch.Internal.Term (Term)
import Plutarch.LedgerApi.AssocMap qualified as AssocMap
import Plutarch.LedgerApi.Utils (pmaybeDataToMaybe, pmaybeToMaybeData)
import Plutarch.LedgerApi.V3 (
  PAddress (PAddress),
  PCredential (PScriptCredential),
  PCurrencySymbol (PCurrencySymbol),
  PDatum (PDatum),
  PDatumHash,
  PMap (PMap),
  POutputDatum (POutputDatumHash),
  PScriptContext (PScriptContext),
  PScriptHash (PScriptHash),
  PScriptInfo (PMintingScript, PSpendingScript),
  PTxInInfo (PTxInInfo),
  PTxInfo (PTxInfo),
  PTxOut (PTxOut),
  PValue (PValue),
 )
import Plutarch.LedgerApi.Value (padaSymbol, padaToken)
import Plutarch.Monadic qualified as P
import Plutarch.Prelude (
  ClosedTerm,
  PAsData,
  PBool (PFalse, PTrue),
  PData,
  PEq ((#==)),
  PInteger,
  PIsData,
  PListLike (phead),
  PMaybe (PJust, PNothing),
  PTryFrom,
  PUnit (PUnit),
  PlutusType,
  S,
  pcon,
  perror,
  pfilter,
  pfoldr,
  pfromData,
  pfstBuiltin,
  pif,
  plam,
  plength,
  pmap,
  pmatch,
  pnot,
  ptraceInfoIfFalse,
  ptryFrom,
  tcont,
  unTermCont,
  (#),
  (#$),
  (#&&),
  (:-->),
 )
import Plutarch.Repr.Data (DeriveAsDataRec (DeriveAsDataRec))
import PlutusLedgerApi.V3 (DatumHash, FromData (fromBuiltinData), ScriptHash, toBuiltin, toData)
import PlutusTx (ToData (toBuiltinData))
import PlutusTx qualified
import PlutusTx.Builtins (chooseData, unsafeDataAsList)

{- | Haskell-level datum for the Proxy Validator script.

 @since 0.1.0
-}
data ProxyDatum = ProxyDatum
  { pdReceiverScript :: ScriptHash
  , pdDatumHash :: DatumHash
  }
  deriving stock (Show, GHC.Generic)
  deriving anyclass (SOP.Generic)

instance PlutusTx.FromData ProxyDatum where
  {-# INLINEABLE fromBuiltinData #-}
  fromBuiltinData d =
    chooseData
      d
      (const Nothing)
      (const Nothing)
      ( \d' ->
          case unsafeDataAsList d' of
            [receiverScript, datumHash] ->
              ProxyDatum
                <$> fromBuiltinData receiverScript
                <*> fromBuiltinData datumHash
            _ -> Nothing
      )
      (const Nothing)
      (const Nothing)
      d

instance PlutusTx.ToData ProxyDatum where
  {-# INLINEABLE toBuiltinData #-}
  toBuiltinData (ProxyDatum receiverScript datumHash) =
    toBuiltin $ PlutusTx.List [toData receiverScript, toData datumHash]

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
proxyScript = plam $ \authSymbol' ctx -> P.do
  PScriptContext txInfo _redeemer scriptInfo <- pmatch $ pfromData ctx

  PTxInfo inputs' _ outputs _ mint' txCerts _ _ _ _ _ _ _ _ _ _ <- pmatch txInfo

  let mint = pfromData mint'
  let inputs = pmap # plam pfromData # pfromData inputs'

  let valid =
        pmatch scriptInfo $ \case
          PSpendingScript txOutRef mayDatum -> P.do
            let authSymbol = pfromData authSymbol'

            -- Spending Condition 1: Transaction burns one GAT (symbol is known from script parameter)
            -- Spending Condition 2: Spent UTxO contains GAT
            let singleAuthTokenBurned = singleAuthorityTokenBurned authSymbol inputs mint

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
            -- Spending Condition 3: Transaction creates a UTxO at address of
            -- receiverScript with empty staking part and reference datum
            -- with hash equal to datumHash.
            let singleOutputWithDatum =
                  ptraceInfoIfFalse "Exactly one output at the receiver script address should exist with datum hash defined in ProxyDatum." $
                    outputsAtReceiver #== 1

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

            let ownScriptHash =
                  phead
                    #$ pmap
                    # plam
                      ( \input -> pmatch input $ \case
                          PTxInInfo _ resolved -> P.do
                            PTxOut addr _ _ _ <- pmatch resolved
                            PAddress cred _ <- pmatch addr
                            PScriptCredential scriptHash <- pmatch cred
                            pfromData scriptHash
                      )
                    #$ pfilter
                    # plam
                      ( \input -> pmatch input $ \case
                          PTxInInfo txOutRef' _ -> txOutRef' #== txOutRef
                      )
                    # scriptInputs

            PScriptHash rawScriptHash <- pmatch ownScriptHash

            let ownCurrencySymbol = pcon $ PCurrencySymbol rawScriptHash

            let mintedGAT3 = psymbolValueOf # ownCurrencySymbol # mint

            PValue mintAssetMap <- pmatch mint
            PMap mintAssetList <- pmatch mintAssetMap

            let tokenMint =
                  pfilter
                    # plam (\mint -> pnot #$ padaSymbol #== pfromData (pfstBuiltin # mint))
                    # mintAssetList

            -- Spending condition 4: Transaction does not mint or burn any tokens other than V2 and V3 GATs.
            let singleGat3Minted =
                  ptraceInfoIfFalse "Transaction must mint a single V3 Authority token (GAT)" $
                    mintedGAT3 #== 1

            -- Previous checks verified that GAT V2 and V3 are present in the transaction, which means that
            -- there should be exactly two currency symbols in the value
            let onlyGatsMinted =
                  ptraceInfoIfFalse "Transaction must not mint or burn anything else than GAT tokens" $
                    (plength # tokenMint) #== 2

            -- Spending condition 5: Transaction does not include any certificates
            let noCertsIncluded =
                  ptraceInfoIfFalse "Transaction must not include any certificates" $
                    (plength # pfromData txCerts) #== 0

            -- Spending condition 6: Transaction does not include script inputs other than own input.
            let singleScriptInput =
                  ptraceInfoIfFalse "Transaction must not include script inputs other than own input." $
                    (plength # scriptInputs) #== 1

            foldr1
              (#&&)
              [ singleAuthTokenBurned
              , singleOutputWithDatum
              , noCertsIncluded
              , singleScriptInput
              , singleGat3Minted
              , onlyGatsMinted
              ]
          PMintingScript currencySymbol -> P.do
            let ownCurrencySymbol = pfromData currencySymbol
            PCurrencySymbol rawScriptHash <- pmatch ownCurrencySymbol
            let ownScriptHash = pcon $ PScriptHash rawScriptHash

            -- let own = pfilterScriptInputs # inputs
            let proxyValidatorInputs =
                  pfilter
                    # plam
                      ( \input -> pmatch input $ \case
                          PTxInInfo _ resolved -> P.do
                            PTxOut addr _ _ _ <- pmatch resolved
                            PAddress cred _ <- pmatch addr
                            pmatch cred $ \case
                              PScriptCredential scriptHash ->
                                pfromData scriptHash #== ownScriptHash
                              _ -> pcon PFalse
                      )
                    # inputs

            -- Spending Condition 1: Transaction contains an input from Proxy Spending Validator.
            let hasProxyValidatorInput =
                  ptraceInfoIfFalse "Transaction must contain an input from Proxy Validator." $
                    (plength # proxyValidatorInputs) #== 1

            PValue mintAssetMap <- pmatch mint

            PJust assets <- pmatch $ AssocMap.plookup # ownCurrencySymbol # mintAssetMap
            PJust mintedGAT3 <- pmatch $ AssocMap.plookup # padaToken # assets

            PMap assetList <- pmatch assets

            -- Spending Condition 2: Transaction mints only one token with currency symbol equal to own hash.
            -- Spending Condition 3: Minted token has empty token name.
            let singleGat3Minted =
                  ptraceInfoIfFalse "Transaction must mint a single V3 Authority token (GAT) with empty token name" $
                    mintedGAT3 #== 1 #&& (plength # assetList #== 1)

            foldr1
              (#&&)
              [ hasProxyValidatorInput
              , singleGat3Minted
              ]
          _ -> pcon PFalse

  pif valid (pcon PUnit) perror
