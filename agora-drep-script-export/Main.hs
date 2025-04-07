module Main (main) where

import Agora.Effects.Voting (votingEffectValidator)
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

compileAndSerialise :: (MonadFail m) => Config -> ClosedTerm a -> m ByteString
compileAndSerialise config script = do
  Script compiled <- case compile config script of
    Right compiled -> pure compiled
    Left e -> fail $ show e
  pure $ CBOR.serialize' $ serialiseUPLC compiled

writeScriptToFile :: FilePath -> ClosedTerm p -> IO ()
writeScriptToFile fp script = do
  putStr ("Exporting " <> fp <> " ...")
  hFlush stdout

  compiledRelease <- compileAndSerialise NoTracing script
  ByteString.writeFile (fp <> ".bin") compiledRelease

  compiledDebug <- compileAndSerialise (Tracing LogDebug DoTracingAndBinds) script
  ByteString.writeFile (fp <> ".debug.bin") compiledDebug

  putStrLn " done"

main :: IO ()
main = do
  putStrLn "Serialising scripts..."
  createDirectoryIfMissing True "./scripts"
  createDirectoryIfMissing True "./scripts/agora"

  let writeAgoraScriptToFile fp = writeScriptToFile ("./scripts/agora/" <> fp)

  writeAgoraScriptToFile "proxyScript" proxyScript
  writeAgoraScriptToFile "votingEffectScript" votingEffectValidator
