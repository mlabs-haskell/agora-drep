{- | Utility to export scripts as files

@since 1.0.0
-}
module Main (main) where

import Agora.Effect.Voting (votingEffectScript)
import Agora.Proxy (proxyScript)
import Cardano.Binary qualified as CBOR
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Plutarch.Internal.Term (
  Config (NoTracing, Tracing),
  LogLevel (LogDebug),
  TracingMode (DoTracingAndBinds),
  compile,
 )
import Plutarch.Prelude (ClosedTerm)
import Plutarch.Script (Script (Script))
import PlutusLedgerApi.Common (serialiseUPLC)
import System.Directory (createDirectoryIfMissing)
import System.IO (hFlush, stdout)

{- | Compile and serialise a Plutarch Term into a bytestring

@since 1.0.0
-}
compileAndSerialise :: (MonadFail m) => Config -> ClosedTerm a -> m ByteString
compileAndSerialise config script = do
  Script compiled <- case compile config script of
    Right compiled -> pure compiled
    Left e -> fail $ show e
  pure $ CBOR.serialize' $ serialiseUPLC compiled

{- | Compile, serialise and write a Plutarch Term to a file

@since 1.0.0
-}
writeScriptToFile :: FilePath -> ClosedTerm p -> IO ()
writeScriptToFile fp script = do
  putStr ("Exporting " <> fp <> " ...")
  hFlush stdout

  compiledRelease <- compileAndSerialise NoTracing script
  ByteString.writeFile (fp <> ".bin") compiledRelease

  compiledDebug <- compileAndSerialise (Tracing LogDebug DoTracingAndBinds) script
  ByteString.writeFile (fp <> ".debug.bin") compiledDebug

  putStrLn " done"

{- | Export all Agora DRep scripts to files under `scripts`

@since 1.0.0
-}
main :: IO ()
main = do
  putStrLn "Serialising scripts..."
  createDirectoryIfMissing True "./scripts"
  createDirectoryIfMissing True "./scripts/agora"

  let writeAgoraScriptToFile fp = writeScriptToFile ("./scripts/agora/" <> fp)

  writeAgoraScriptToFile "proxyScript" proxyScript
  writeAgoraScriptToFile "votingEffectScript" votingEffectScript
