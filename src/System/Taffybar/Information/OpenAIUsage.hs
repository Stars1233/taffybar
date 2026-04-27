{-# LANGUAGE OverloadedStrings #-}

-- | Shared OpenAI Codex subscription usage information.
--
-- This module currently reads the ChatGPT OAuth token file written by Codex and
-- calls ChatGPT's Codex usage endpoint. That endpoint is not part of OpenAI's
-- public API, so callers can override the endpoint, auth path, and user agent.
module System.Taffybar.Information.OpenAIUsage
  ( OpenAIUsageConfig (..),
    defaultOpenAIUsageConfig,
    OpenAIUsageAuth (..),
    OpenAIUsageSnapshot (..),
    OpenAIUsageInfo (..),
    OpenAIUsageAdditionalRateLimit (..),
    OpenAIUsageRateLimit (..),
    OpenAIUsageWindow (..),
    OpenAIUsageCredits (..),
    getOpenAIUsageInfo,
    pollOpenAIUsage,
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
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.HTTP.Simple
import System.Directory (doesFileExist, getHomeDirectory)
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import System.Log.Logger (Priority (WARNING), logM)
import System.Taffybar.Context (TaffyIO, getStateDefault)
import System.Taffybar.Information.Wakeup (getWakeupChannelForDelay)

data OpenAIUsageConfig = OpenAIUsageConfig
  { openAIUsagePollInterval :: Double,
    openAIUsageEndpoint :: String,
    openAIUsageUserAgent :: String,
    openAIUsageAuthPath :: Maybe FilePath
  }

defaultOpenAIUsageConfig :: OpenAIUsageConfig
defaultOpenAIUsageConfig =
  OpenAIUsageConfig
    { openAIUsagePollInterval = 60 * 15,
      openAIUsageEndpoint = "https://chatgpt.com/backend-api/codex/usage",
      openAIUsageUserAgent = "taffybar-openai-usage",
      openAIUsageAuthPath = Nothing
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
    openAIUsageResetAfterSeconds :: Maybe Int
  }
  deriving (Eq, Show)

instance FromJSON OpenAIUsageWindow where
  parseJSON = withObject "OpenAIUsageWindow" $ \o ->
    OpenAIUsageWindow
      <$> o .: "used_percent"
      <*> o .:? "limit_window_seconds"
      <*> o .:? "reset_after_seconds"

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
  auth <- readOpenAIUsageAuth config
  fetchOpenAIUsage config auth

pollOpenAIUsage :: OpenAIUsageConfig -> IO OpenAIUsageSnapshot
pollOpenAIUsage config = do
  result <- try $ getOpenAIUsageInfo config
  case result of
    Right info -> return $ OpenAIUsageAvailable info
    Left (err :: SomeException) -> do
      let message = T.pack $ show err
      logM logName WARNING $ "OpenAI usage poll failed: " <> show err
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
  initial <- liftIO $ pollOpenAIUsage config
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
          updateOpenAIUsage config chan var
  return $ OpenAIUsageChanVar (chan, var, refreshChan)

updateOpenAIUsage ::
  OpenAIUsageConfig ->
  TChan OpenAIUsageSnapshot ->
  MVar OpenAIUsageSnapshot ->
  IO ()
updateOpenAIUsage config chan var = do
  snapshot <- pollOpenAIUsage config
  void $ swapMVar var snapshot
  atomically $ writeTChan chan snapshot

logName :: String
logName = "System.Taffybar.Information.OpenAIUsage"
