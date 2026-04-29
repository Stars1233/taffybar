{-# LANGUAGE OverloadedStrings #-}

-- | Shared OpenAI Codex subscription usage information.
--
-- This module currently reads the ChatGPT OAuth token file written by Codex and
-- calls ChatGPT's Codex usage endpoint. That endpoint is not part of OpenAI's
-- public API, so callers can override the endpoint, auth path, and user agent.
-- Local Codex transcript JSONL files are used to derive actual token counts,
-- because the remote endpoint currently exposes percentages and reset times but
-- not token totals.
module System.Taffybar.Information.OpenAIUsage
  ( OpenAIUsageConfig (..),
    defaultOpenAIUsageConfig,
    OpenAIUsageAuth (..),
    OpenAIUsageSnapshot (..),
    OpenAIUsageInfo (..),
    OpenAIUsageAdditionalRateLimit (..),
    OpenAIUsageRateLimit (..),
    OpenAIUsageWindow (..),
    OpenAIUsageTotals (..),
    OpenAIUsageCredits (..),
    getOpenAIUsageInfo,
    updateOpenAIUsage,
    getOpenAIUsageChan,
    getOpenAIUsageState,
    forceOpenAIUsageRefresh,
  )
where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
import Control.Concurrent.STM (atomically, orElse)
import Control.Concurrent.STM.TChan
import Control.Exception (SomeException, try)
import Control.Monad (forever, void)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Data.Aeson.Types (Parser)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBS8
import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Clock
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Network.HTTP.Simple
import System.Directory
  ( doesDirectoryExist,
    doesFileExist,
    getHomeDirectory,
    getModificationTime,
    listDirectory,
  )
import System.Environment (lookupEnv)
import System.FilePath (takeExtension, (</>))
import System.Log.Logger (Priority (WARNING), logM)
import System.Taffybar.Context (TaffyIO, getStateDefault)
import System.Taffybar.Information.Wakeup (getWakeupChannelForDelay)

data OpenAIUsageConfig = OpenAIUsageConfig
  { openAIUsagePollInterval :: Double,
    openAIUsageEndpoint :: String,
    openAIUsageUserAgent :: String,
    openAIUsageAuthPath :: Maybe FilePath,
    openAIUsageSessionsPath :: Maybe FilePath,
    openAIUsageFileLookbackSeconds :: NominalDiffTime
  }

defaultOpenAIUsageConfig :: OpenAIUsageConfig
defaultOpenAIUsageConfig =
  OpenAIUsageConfig
    { openAIUsagePollInterval = 60 * 15,
      openAIUsageEndpoint = "https://chatgpt.com/backend-api/codex/usage",
      openAIUsageUserAgent = "taffybar-openai-usage",
      openAIUsageAuthPath = Nothing,
      openAIUsageSessionsPath = Nothing,
      openAIUsageFileLookbackSeconds = 8 * 24 * 60 * 60
    }

data OpenAIUsageAuth = OpenAIUsageAuth
  { openAIUsageAccessToken :: T.Text,
    openAIUsageAccountId :: T.Text
  }

instance FromJSON OpenAIUsageAuth where
  parseJSON = withObject "OpenAIUsageAuth" $ \root -> do
    tokens <- root .: "tokens"
    OpenAIUsageAuth
      <$> tokens .: "access_token"
      <*> tokens .: "account_id"

data OpenAIUsageSnapshot
  = OpenAIUsageAvailable OpenAIUsageInfo
  | OpenAIUsageUnavailable T.Text
  deriving (Eq, Show)

data OpenAIUsageInfo = OpenAIUsageInfo
  { openAIUsagePlanType :: Maybe T.Text,
    openAIUsageRateLimit :: OpenAIUsageRateLimit,
    openAIUsageAdditionalRateLimits :: [OpenAIUsageAdditionalRateLimit],
    openAIUsageCredits :: Maybe OpenAIUsageCredits,
    openAIUsageReachedType :: Maybe T.Text
  }
  deriving (Eq, Show)

instance FromJSON OpenAIUsageInfo where
  parseJSON = withObject "OpenAIUsageInfo" $ \o ->
    OpenAIUsageInfo
      <$> o .:? "plan_type"
      <*> o .: "rate_limit"
      <*> o .:? "additional_rate_limits" .!= []
      <*> o .:? "credits"
      <*> o .:? "rate_limit_reached_type"

data OpenAIUsageAdditionalRateLimit = OpenAIUsageAdditionalRateLimit
  { openAIUsageAdditionalLimitName :: Maybe T.Text,
    openAIUsageAdditionalRateLimit :: OpenAIUsageRateLimit
  }
  deriving (Eq, Show)

instance FromJSON OpenAIUsageAdditionalRateLimit where
  parseJSON = withObject "OpenAIUsageAdditionalRateLimit" $ \o ->
    OpenAIUsageAdditionalRateLimit
      <$> o .:? "limit_name"
      <*> o .: "rate_limit"

data OpenAIUsageRateLimit = OpenAIUsageRateLimit
  { openAIUsageAllowed :: Bool,
    openAIUsageLimitReached :: Bool,
    openAIUsagePrimaryWindow :: Maybe OpenAIUsageWindow,
    openAIUsageSecondaryWindow :: Maybe OpenAIUsageWindow
  }
  deriving (Eq, Show)

instance FromJSON OpenAIUsageRateLimit where
  parseJSON = withObject "OpenAIUsageRateLimit" $ \o ->
    OpenAIUsageRateLimit
      <$> o .:? "allowed" .!= True
      <*> o .:? "limit_reached" .!= False
      <*> o .:? "primary_window"
      <*> o .:? "secondary_window"

data OpenAIUsageWindow = OpenAIUsageWindow
  { openAIUsageUsedPercent :: Int,
    openAIUsageWindowDurationSeconds :: Maybe Int,
    openAIUsageResetAfterSeconds :: Maybe Int,
    openAIUsageResetAt :: Maybe UTCTime,
    openAIUsageWindowTotals :: Maybe OpenAIUsageTotals
  }
  deriving (Eq, Show)

instance FromJSON OpenAIUsageWindow where
  parseJSON = withObject "OpenAIUsageWindow" $ \o -> do
    resetAt <- (o .:? "reset_at" :: Parser (Maybe Int))
    OpenAIUsageWindow
      <$> o .: "used_percent"
      <*> o .:? "limit_window_seconds"
      <*> o .:? "reset_after_seconds"
      <*> pure (posixSecondsToUTCTime . realToFrac <$> (resetAt :: Maybe Int))
      <*> pure Nothing

data OpenAIUsageTotals = OpenAIUsageTotals
  { openAIUsageRequestCount :: Int,
    openAIUsageInputTokens :: Int,
    openAIUsageCachedInputTokens :: Int,
    openAIUsageOutputTokens :: Int,
    openAIUsageReasoningOutputTokens :: Int,
    openAIUsageTotalTokens :: Int
  }
  deriving (Eq, Show)

instance Semigroup OpenAIUsageTotals where
  a <> b =
    OpenAIUsageTotals
      { openAIUsageRequestCount = openAIUsageRequestCount a + openAIUsageRequestCount b,
        openAIUsageInputTokens = openAIUsageInputTokens a + openAIUsageInputTokens b,
        openAIUsageCachedInputTokens = openAIUsageCachedInputTokens a + openAIUsageCachedInputTokens b,
        openAIUsageOutputTokens = openAIUsageOutputTokens a + openAIUsageOutputTokens b,
        openAIUsageReasoningOutputTokens = openAIUsageReasoningOutputTokens a + openAIUsageReasoningOutputTokens b,
        openAIUsageTotalTokens = openAIUsageTotalTokens a + openAIUsageTotalTokens b
      }

instance Monoid OpenAIUsageTotals where
  mempty =
    OpenAIUsageTotals
      { openAIUsageRequestCount = 0,
        openAIUsageInputTokens = 0,
        openAIUsageCachedInputTokens = 0,
        openAIUsageOutputTokens = 0,
        openAIUsageReasoningOutputTokens = 0,
        openAIUsageTotalTokens = 0
      }

instance FromJSON OpenAIUsageTotals where
  parseJSON = withObject "OpenAIUsageTotals" $ \o -> do
    input <- o .:? "input_tokens" .!= 0
    cachedInput <- o .:? "cached_input_tokens" .!= 0
    outputTokens <- o .:? "output_tokens" .!= 0
    reasoningOutput <- o .:? "reasoning_output_tokens" .!= 0
    total <- o .:? "total_tokens" .!= (input + outputTokens)
    return $
      OpenAIUsageTotals
        { openAIUsageRequestCount = 1,
          openAIUsageInputTokens = input,
          openAIUsageCachedInputTokens = cachedInput,
          openAIUsageOutputTokens = outputTokens,
          openAIUsageReasoningOutputTokens = reasoningOutput,
          openAIUsageTotalTokens = total
        }

data OpenAIUsageCredits = OpenAIUsageCredits
  { openAIUsageHasCredits :: Bool,
    openAIUsageCreditsUnlimited :: Bool,
    openAIUsageCreditsBalance :: Maybe T.Text
  }
  deriving (Eq, Show)

instance FromJSON OpenAIUsageCredits where
  parseJSON = withObject "OpenAIUsageCredits" $ \o ->
    OpenAIUsageCredits
      <$> o .:? "has_credits" .!= False
      <*> o .:? "unlimited" .!= False
      <*> o .:? "balance"

getOpenAIUsageInfo :: OpenAIUsageConfig -> IO OpenAIUsageInfo
getOpenAIUsageInfo config = do
  now <- getCurrentTime
  auth <- readOpenAIUsageAuth config
  info <- fetchOpenAIUsage config auth
  entries <- readOpenAITranscriptEntries config now
  return $ addOpenAITranscriptTotals entries info

updateOpenAIUsage :: OpenAIUsageConfig -> IO OpenAIUsageSnapshot
updateOpenAIUsage config = do
  result <- try $ getOpenAIUsageInfo config
  case result of
    Right info -> return $ OpenAIUsageAvailable info
    Left (err :: SomeException) -> do
      let message = T.pack $ show err
      logM logName WARNING $ "OpenAI usage update failed: " <> show err
      return $ OpenAIUsageUnavailable message

readOpenAIUsageAuth :: OpenAIUsageConfig -> IO OpenAIUsageAuth
readOpenAIUsageAuth config = do
  path <- maybe defaultCodexAuthPath return (openAIUsageAuthPath config)
  bytes <- LBS.readFile path
  either fail return $ eitherDecode bytes

defaultCodexAuthPath :: IO FilePath
defaultCodexAuthPath = do
  codexHome <- lookupEnv "CODEX_HOME"
  case codexHome of
    Just dir -> return $ dir </> "auth.json"
    Nothing -> do
      home <- getHomeDirectory
      let path = home </> ".codex" </> "auth.json"
      exists <- doesFileExist path
      if exists
        then return path
        else fail "No Codex auth file found"

fetchOpenAIUsage :: OpenAIUsageConfig -> OpenAIUsageAuth -> IO OpenAIUsageInfo
fetchOpenAIUsage config auth = do
  request0 <- parseRequest $ openAIUsageEndpoint config
  let request =
        setRequestHeader "Authorization" ["Bearer " <> TE.encodeUtf8 (openAIUsageAccessToken auth)] $
          setRequestHeader "ChatGPT-Account-Id" [TE.encodeUtf8 (openAIUsageAccountId auth)] $
            setRequestHeader "Accept" ["application/json"] $
              setRequestHeader "User-Agent" [TE.encodeUtf8 (T.pack (openAIUsageUserAgent config))] request0
  response <- httpLBS request
  let statusCode = getResponseStatusCode response
      body = getResponseBody response
  if statusCode >= 200 && statusCode < 300
    then either fail return $ eitherDecode body
    else fail $ "OpenAI usage endpoint returned HTTP " <> show statusCode

data OpenAITranscriptEntry = OpenAITranscriptEntry
  { openAITranscriptTimestamp :: UTCTime,
    openAITranscriptTotals :: OpenAIUsageTotals
  }
  deriving (Eq, Show)

data OpenAITranscriptJSON = OpenAITranscriptJSON
  { openAITranscriptJSONTimestamp :: Maybe UTCTime,
    openAITranscriptJSONUsage :: Maybe OpenAIUsageTotals
  }

instance FromJSON OpenAITranscriptJSON where
  parseJSON = withObject "OpenAITranscriptJSON" $ \o -> do
    payload <- o .:? "payload"
    usage <- maybe (return Nothing) parseTokenCountPayload payload
    OpenAITranscriptJSON
      <$> o .:? "timestamp"
      <*> pure usage

parseTokenCountPayload :: Value -> Parser (Maybe OpenAIUsageTotals)
parseTokenCountPayload =
  withObject "TokenCountPayload" $ \payload -> do
    payloadType <- payload .:? "type" :: Parser (Maybe T.Text)
    case payloadType of
      Just "token_count" -> payload .:? "info" >>= traverse parseTokenCountInfo
      _ -> return Nothing

parseTokenCountInfo :: Value -> Parser OpenAIUsageTotals
parseTokenCountInfo =
  withObject "TokenCountInfo" $ \info ->
    info .: "last_token_usage"

readOpenAITranscriptEntries :: OpenAIUsageConfig -> UTCTime -> IO [OpenAITranscriptEntry]
readOpenAITranscriptEntries config now = do
  sessionsPath <- maybe defaultCodexSessionsPath return (openAIUsageSessionsPath config)
  exists <- doesDirectoryExist sessionsPath
  if not exists
    then return []
    else do
      files <- jsonlFilesModifiedSince (addUTCTime (negate $ openAIUsageFileLookbackSeconds config) now) sessionsPath
      concat <$> mapM readTranscriptFile files

defaultCodexSessionsPath :: IO FilePath
defaultCodexSessionsPath = do
  codexHome <- lookupEnv "CODEX_HOME"
  case codexHome of
    Just dir -> return $ dir </> "sessions"
    Nothing -> do
      home <- getHomeDirectory
      return $ home </> ".codex" </> "sessions"

jsonlFilesModifiedSince :: UTCTime -> FilePath -> IO [FilePath]
jsonlFilesModifiedSince cutoff path = do
  entries <- listDirectory path
  concat
    <$> mapM
      ( \entry -> do
          let child = path </> entry
          isDirectory <- doesDirectoryExist child
          isFile <- doesFileExist child
          if isDirectory
            then jsonlFilesModifiedSince cutoff child
            else
              if isFile && takeExtension child == ".jsonl"
                then do
                  modified <- getModificationTime child
                  return [child | modified >= cutoff]
                else return []
      )
      entries

readTranscriptFile :: FilePath -> IO [OpenAITranscriptEntry]
readTranscriptFile path = do
  bytes <- LBS8.readFile path
  return $ mapMaybe transcriptLineToEntry (LBS8.lines bytes)

transcriptLineToEntry :: LBS.ByteString -> Maybe OpenAITranscriptEntry
transcriptLineToEntry bytes = do
  decoded <- either (const Nothing) Just $ eitherDecode bytes
  timestamp <- openAITranscriptJSONTimestamp decoded
  usage <- openAITranscriptJSONUsage decoded
  return $
    OpenAITranscriptEntry
      { openAITranscriptTimestamp = timestamp,
        openAITranscriptTotals = usage
      }

addOpenAITranscriptTotals :: [OpenAITranscriptEntry] -> OpenAIUsageInfo -> OpenAIUsageInfo
addOpenAITranscriptTotals entries info =
  info
    { openAIUsageRateLimit = addRateLimitTranscriptTotals entries (openAIUsageRateLimit info)
    }

addRateLimitTranscriptTotals :: [OpenAITranscriptEntry] -> OpenAIUsageRateLimit -> OpenAIUsageRateLimit
addRateLimitTranscriptTotals entries limit =
  limit
    { openAIUsagePrimaryWindow = addWindowTranscriptTotals entries <$> openAIUsagePrimaryWindow limit,
      openAIUsageSecondaryWindow = addWindowTranscriptTotals entries <$> openAIUsageSecondaryWindow limit
    }

addWindowTranscriptTotals :: [OpenAITranscriptEntry] -> OpenAIUsageWindow -> OpenAIUsageWindow
addWindowTranscriptTotals entries window =
  window
    { openAIUsageWindowTotals =
        foldMap openAITranscriptTotals <$> windowEntries window entries
    }

windowEntries :: OpenAIUsageWindow -> [OpenAITranscriptEntry] -> Maybe [OpenAITranscriptEntry]
windowEntries window entries = do
  duration <- openAIUsageWindowDurationSeconds window
  end <- openAIUsageResetAt window
  let start = addUTCTime (negate $ fromIntegral duration) end
  return $
    filter
      ( \entry ->
          let timestamp = openAITranscriptTimestamp entry
           in timestamp >= start && timestamp < end
      )
      entries

newtype OpenAIUsageChanVar
  = OpenAIUsageChanVar
      ( TChan OpenAIUsageSnapshot,
        MVar OpenAIUsageSnapshot,
        TChan ()
      )

getOpenAIUsageChan :: OpenAIUsageConfig -> TaffyIO (TChan OpenAIUsageSnapshot)
getOpenAIUsageChan config = do
  OpenAIUsageChanVar (chan, _, _) <- setupOpenAIUsageChanVar config
  return chan

getOpenAIUsageState :: OpenAIUsageConfig -> TaffyIO OpenAIUsageSnapshot
getOpenAIUsageState config = do
  OpenAIUsageChanVar (_, var, _) <- setupOpenAIUsageChanVar config
  liftIO $ readMVar var

forceOpenAIUsageRefresh :: OpenAIUsageConfig -> TaffyIO ()
forceOpenAIUsageRefresh config = do
  OpenAIUsageChanVar (_, _, refreshChan) <- setupOpenAIUsageChanVar config
  liftIO $ atomically $ writeTChan refreshChan ()

setupOpenAIUsageChanVar :: OpenAIUsageConfig -> TaffyIO OpenAIUsageChanVar
setupOpenAIUsageChanVar config = getStateDefault $ do
  chan <- liftIO newBroadcastTChanIO
  refreshChan <- liftIO newTChanIO
  initial <- liftIO $ updateOpenAIUsage config
  var <- liftIO $ newMVar initial
  wakeupChan <- getWakeupChannelForDelay (openAIUsagePollInterval config)
  ourWakeupChan <- liftIO $ atomically $ dupTChan wakeupChan
  void $
    liftIO $
      forkIO $
        forever $ do
          atomically $
            void (readTChan refreshChan)
              `orElse` void (readTChan ourWakeupChan)
          refreshOpenAIUsageState config chan var
  return $ OpenAIUsageChanVar (chan, var, refreshChan)

refreshOpenAIUsageState ::
  OpenAIUsageConfig ->
  TChan OpenAIUsageSnapshot ->
  MVar OpenAIUsageSnapshot ->
  IO ()
refreshOpenAIUsageState config chan var = do
  snapshot <- updateOpenAIUsage config
  void $ swapMVar var snapshot
  atomically $ writeTChan chan snapshot

logName :: String
logName = "System.Taffybar.Information.OpenAIUsage"
