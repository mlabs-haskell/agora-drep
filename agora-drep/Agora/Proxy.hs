module Agora.Proxy (proxyScript, PProxyDatum (..), ProxyDatum (..)) where

import Agora.Utils (pcountIf, pcurrencySymbolToScriptHash, pscriptHashToCurrencySymbol)
import Data.Kind (Type)
import GHC.Generics qualified as GHC
import Generics.SOP qualified as SOP
import Plutarch.Internal.Term (Term)
import Plutarch.LedgerApi.Utils (PMaybeData (PDJust))
import Plutarch.LedgerApi.V3 (
  PAddress (PAddress),
  PCredential (PScriptCredential),
  PCurrencySymbol,
  PDatum (PDatum),
  PDatumHash,
  PMap (PMap),
  POutputDatum (POutputDatumHash),
  PScriptContext (PScriptContext),
  PScriptHash,
  PScriptInfo (PMintingScript, PSpendingScript),
  PTxInInfo (PTxInInfo),
  PTxInfo (PTxInfo),
  PTxOut (PTxOut),
  PValue (PValue),
 )
import Plutarch.Maybe (PMaybe (PNothing), pjust)
import Plutarch.Monadic qualified as P
import Plutarch.Prelude (
  ClosedTerm,
  PAsData,
  PBool (PFalse),
  PBuiltinList (PCons, PNil),
  PData,
  PEq ((#==)),
  PInteger,
  PIsData,
  PMaybe (PJust),
  PTryFrom,
  PUnit (PUnit),
  PlutusType,
  S,
  pcon,
  pconstant,
  perror,
  pfoldr,
  pfromData,
  pfstBuiltin,
  pif,
  plam,
  plet,
  pmatch,
  precList,
  psndBuiltin,
  ptraceInfoIfFalse,
  ptryFrom,
  tcont,
  unTermCont,
  (#),
  (#&&),
  (:-->),
 )
import Plutarch.Repr.Data (DeriveAsDataRec (DeriveAsDataRec))
import PlutusLedgerApi.V3 (DatumHash, FromData (fromBuiltinData), ScriptHash, adaToken, toBuiltin, toData)
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

  -- When this script runs also V2 GAT gets burned so that guarantees that no V3 thing happens
  PTxInfo inputs _ outputs _ mint' txCerts _ _ _ _ _ _ _ _ _ _ <- pmatch txInfo

  let mint = pfromData mint'
  -- Pattern matching asserts that there are only two symbols minted and `mintCheck`
  -- asserts that they are V2 and V3 gats.
  PValue mintAssetMap <- pmatch mint
  PMap mintAssetList <- pmatch mintAssetMap
  PCons mintAssetPair1 mintAssetListRest1 <- pmatch mintAssetList
  PCons mintAssetPair2 mintAssetListRest2 <- pmatch mintAssetListRest1
  PNil <- pmatch mintAssetListRest2
  let mintCs1 = pfstBuiltin # mintAssetPair1
  PMap mintTokens1 <- pmatch $ pfromData (psndBuiltin # mintAssetPair1)
  PCons mintTokenPair1 mintTokens1Rest <- pmatch mintTokens1
  PNil <- pmatch mintTokens1Rest
  let mintCs2 = pfstBuiltin # mintAssetPair2
  PMap mintTokens2 <- pmatch $ pfromData (psndBuiltin # mintAssetPair2)
  PCons mintTokenPair2 mintTokens2Rest <- pmatch mintTokens2
  PNil <- pmatch mintTokens2Rest

  let valid =
        pmatch scriptInfo $ \case
          PSpendingScript _txOutRef mayDatum -> P.do
            PDJust datum <- pmatch mayDatum
            PDatum rawDatum <- pmatch (pfromData datum)

            let rawProxyDatum = unTermCont $ fst <$> tcont (ptryFrom @(PAsData PProxyDatum) rawDatum)
            PProxyDatum receiverScript datumHash <- pmatch $ pfromData rawProxyDatum

            let receiverScriptAddr =
                  pcon $ PAddress (pcon $ PScriptCredential receiverScript) (pconstant Nothing)

            let outputsAtReceiver =
                  pfoldr
                    # plam
                      ( \output v -> P.do
                          PTxOut address _ outputDatum _ <- pmatch (pfromData output)
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
                    # pfromData outputs
            -- Spending Condition 3: Transaction creates a UTxO at address of
            -- receiverScript with empty staking part and reference datum
            -- with hash equal to datumHash.
            let singleOutputWithDatum =
                  ptraceInfoIfFalse "Exactly one output at the receiver script address should exist with datum hash defined in ProxyDatum." $
                    outputsAtReceiver #== 1

            -- If there is only one script input and we are running that means that we are that input
            -- so no need to check explicitly
            let scriptInputs =
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

            -- Spending condition 6: Transaction does not include script inputs other than own input.
            PJust ownScriptHash <- pmatch scriptInputs
            ownCurrencySymbol <- plet $ pscriptHashToCurrencySymbol ownScriptHash

            -- Spending Condition 1: Transaction burns one GAT (symbol is known from script parameter)
            -- Spending Condition 2: Spent UTxO contains GAT
            -- Spending condition 4: Transaction does not mint or burn any tokens other than V2 and V3 GATs.

            -- There is a bit to unpack here:
            -- Fact that we are burning a V2 GAT combined with Condition 6 ensures that there is a
            -- V2 GAT in the inputs. There is only one script input so it either comes from the
            -- currently validated input OR that there is a pubkey address that had V2 GAT.
            -- If it is the latter that means that the whole setup is already compromised so it
            -- does not matter what happens here anyway.
            -- No need to check for name of V2 token as GAT policy enforces that
            let mintCheck =
                  pif
                    (mintCs1 #== ownCurrencySymbol)
                    ( ((pfstBuiltin # mintTokenPair1) #== pconstant adaToken)
                        #&& (pfromData (psndBuiltin # mintTokenPair1) #== pconstant 1)
                        #&& (mintCs2 #== authSymbol')
                        #&& (pfromData (psndBuiltin # mintTokenPair2) #== pconstant (-1))
                    )
                    ( ((pfstBuiltin # mintTokenPair2) #== pconstant adaToken)
                        #&& (pfromData (psndBuiltin # mintTokenPair2) #== pconstant 1)
                        #&& (mintCs2 #== ownCurrencySymbol)
                        #&& (mintCs1 #== authSymbol')
                        #&& (pfromData (psndBuiltin # mintTokenPair1) #== pconstant (-1))
                    )

            -- Spending condition 5: Transaction does not include any certificates
            PNil <- pmatch (pfromData txCerts)

            foldr1
              (#&&)
              [ singleOutputWithDatum
              , mintCheck
              ]
          PMintingScript currencySymbol -> P.do
            let isValidatorInput =
                  plam
                    ( \input -> pmatch (pfromData input) $ \case
                        PTxInInfo _ resolved -> P.do
                          PTxOut addr _ _ _ <- pmatch resolved
                          PAddress cred _ <- pmatch addr
                          pmatch cred $ \case
                            PScriptCredential scriptHash ->
                              scriptHash #== pcurrencySymbolToScriptHash currencySymbol
                            _ -> pcon PFalse
                    )

            -- Spending Condition 1: Transaction contains an input from Proxy Spending Validator.
            ptraceInfoIfFalse "Transaction must contain an input from Proxy Validator." $
              (pcountIf # isValidatorInput # pfromData inputs) #== 1
          _ -> perror

  pif valid (pcon PUnit) perror
