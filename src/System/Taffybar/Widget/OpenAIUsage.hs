{-# LANGUAGE OverloadedStrings #-}

-- | Widget for displaying OpenAI Codex subscription usage.
--
-- This currently uses the same ChatGPT OAuth token file that Codex writes at
-- @$CODEX_HOME/auth.json@ or @~/.codex/auth.json@ and calls ChatGPT's Codex
-- usage endpoint. The endpoint is not part of the public OpenAI API, so the
-- auth and endpoint pieces are configurable.
module System.Taffybar.Widget.OpenAIUsage
  ( OpenAIUsageWidgetConfig (..),
    OpenAIUsageAuth (..),
    defaultOpenAIUsageWidgetConfig,
    openAIUsageNew,
    openAIUsageNewWith,
  )
where

import Control.Exception (SomeException, try)
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe (catMaybes, fromMaybe, isJust, listToMaybe, mapMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import GI.Gtk (Widget)
import Network.HTTP.Simple
import System.Directory (doesFileExist, getHomeDirectory)
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import System.Log.Logger (Priority (ERROR), logM)
import System.Taffybar.Widget.Generic.PollingLabel
  ( pollingLabelWithVariableDelayAndRefresh,
  )
import System.Taffybar.Widget.Util (widgetSetClassGI)
import Text.Printf (printf)

data OpenAIUsageWidgetConfig = OpenAIUsageWidgetConfig
  { openAIUsagePollInterval :: Double,
    openAIUsageEndpoint :: String,
    openAIUsageUserAgent :: String,
    openAIUsageAuthPath :: Maybe FilePath,
    openAIUsageFallbackLabel :: T.Text
  }

defaultOpenAIUsageWidgetConfig :: OpenAIUsageWidgetConfig
defaultOpenAIUsageWidgetConfig =
  OpenAIUsageWidgetConfig
    { openAIUsagePollInterval = 60 * 5,
      openAIUsageEndpoint = "https://chatgpt.com/backend-api/codex/usage",
      openAIUsageUserAgent = "taffybar-openai-usage",
      openAIUsageAuthPath = Nothing,
      openAIUsageFallbackLabel = "AI n/a"
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

data UsageResponse = UsageResponse
  { usagePlanType :: Maybe T.Text,
    usageRateLimit :: RateLimit,
    usageAdditionalRateLimits :: [AdditionalRateLimit],
    usageCredits :: Maybe Credits,
    usageReachedType :: Maybe T.Text
  }

instance FromJSON UsageResponse where
  parseJSON = withObject "UsageResponse" $ \o ->
    UsageResponse
      <$> o .:? "plan_type"
      <*> o .: "rate_limit"
      <*> o .:? "additional_rate_limits" .!= []
      <*> o .:? "credits"
      <*> o .:? "rate_limit_reached_type"

data AdditionalRateLimit = AdditionalRateLimit
  { additionalLimitName :: Maybe T.Text,
    additionalRateLimit :: RateLimit
  }

instance FromJSON AdditionalRateLimit where
  parseJSON = withObject "AdditionalRateLimit" $ \o ->
    AdditionalRateLimit
      <$> o .:? "limit_name"
      <*> o .: "rate_limit"

data RateLimit = RateLimit
  { rateLimitAllowed :: Bool,
    rateLimitReached :: Bool,
    rateLimitPrimaryWindow :: Maybe RateLimitWindow,
    rateLimitSecondaryWindow :: Maybe RateLimitWindow
  }

instance FromJSON RateLimit where
  parseJSON = withObject "RateLimit" $ \o ->
    RateLimit
      <$> o .:? "allowed" .!= True
      <*> o .:? "limit_reached" .!= False
      <*> o .:? "primary_window"
      <*> o .:? "secondary_window"

data RateLimitWindow = RateLimitWindow
  { windowUsedPercent :: Int,
    windowDurationSeconds :: Maybe Int,
    windowResetAfterSeconds :: Maybe Int
  }

instance FromJSON RateLimitWindow where
  parseJSON = withObject "RateLimitWindow" $ \o ->
    RateLimitWindow
      <$> o .: "used_percent"
      <*> o .:? "limit_window_seconds"
      <*> o .:? "reset_after_seconds"

data Credits = Credits
  { creditsHasCredits :: Bool,
    creditsUnlimited :: Bool,
    creditsBalance :: Maybe T.Text
  }

instance FromJSON Credits where
  parseJSON = withObject "Credits" $ \o ->
    Credits
      <$> o .:? "has_credits" .!= False
      <*> o .:? "unlimited" .!= False
      <*> o .:? "balance"

openAIUsageNew :: (MonadIO m) => m Widget
openAIUsageNew = openAIUsageNewWith defaultOpenAIUsageWidgetConfig

openAIUsageNewWith :: (MonadIO m) => OpenAIUsageWidgetConfig -> m Widget
openAIUsageNewWith config =
  pollingLabelWithVariableDelayAndRefresh action True
    >>= (`widgetSetClassGI` "openai-usage")
  where
    action = do
      result <- try $ fetchAndFormatUsage config
      case result of
        Right formatted -> return formatted
        Left (err :: SomeException) -> do
          logM "System.Taffybar.Widget.OpenAIUsage" ERROR (show err)
          return
            ( openAIUsageFallbackLabel config,
              Just "Unable to fetch OpenAI usage",
              min 60 (openAIUsagePollInterval config)
            )

fetchAndFormatUsage :: OpenAIUsageWidgetConfig -> IO (T.Text, Maybe T.Text, Double)
fetchAndFormatUsage config = do
  auth <- readOpenAIUsageAuth config
  usage <- fetchUsage config auth
  let label = formatUsageLabel usage
      tooltip = formatUsageTooltip usage
  return (label, Just tooltip, openAIUsagePollInterval config)

readOpenAIUsageAuth :: OpenAIUsageWidgetConfig -> IO OpenAIUsageAuth
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

fetchUsage :: OpenAIUsageWidgetConfig -> OpenAIUsageAuth -> IO UsageResponse
fetchUsage config auth = do
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

formatUsageLabel :: UsageResponse -> T.Text
formatUsageLabel usage =
  let percentText = maybe "n/a" (T.pack . printf "%d%%") $ usagePercent usage
      reached = rateLimitReached (usageRateLimit usage) || isJust (usageReachedType usage)
   in (if reached then "AI ! " else "AI ") <> percentText

formatUsageTooltip :: UsageResponse -> T.Text
formatUsageTooltip usage =
  T.intercalate "\n" $
    catMaybes
      [ ("Plan: " <>) <$> usagePlanType usage,
        Just $ "Default: " <> formatRateLimit (usageRateLimit usage),
        formatAdditional <$> listToMaybe (usageAdditionalRateLimits usage),
        formatCredits <$> usageCredits usage,
        ("Reached: " <>) <$> usageReachedType usage
      ]

usagePercent :: UsageResponse -> Maybe Int
usagePercent usage =
  maximumMaybe $
    windowPercents (usageRateLimit usage)
      <> concatMap (windowPercents . additionalRateLimit) (usageAdditionalRateLimits usage)

windowPercents :: RateLimit -> [Int]
windowPercents limit =
  map windowUsedPercent $
    catMaybes [rateLimitPrimaryWindow limit, rateLimitSecondaryWindow limit]

maximumMaybe :: (Ord a) => [a] -> Maybe a
maximumMaybe [] = Nothing
maximumMaybe values = Just $ maximum values

formatAdditional :: AdditionalRateLimit -> T.Text
formatAdditional additional =
  fromMaybe "Additional" (additionalLimitName additional)
    <> ": "
    <> formatRateLimit (additionalRateLimit additional)

formatRateLimit :: RateLimit -> T.Text
formatRateLimit limit =
  let windows =
        T.intercalate
          ", "
          ( mapMaybe
              (uncurry formatWindow)
              [ ("short", rateLimitPrimaryWindow limit),
                ("long", rateLimitSecondaryWindow limit)
              ]
          )
      status
        | rateLimitReached limit = "reached"
        | not (rateLimitAllowed limit) = "blocked"
        | otherwise = "allowed"
   in windows <> " (" <> status <> ")"

formatWindow :: T.Text -> Maybe RateLimitWindow -> Maybe T.Text
formatWindow _ Nothing = Nothing
formatWindow name (Just window) =
  Just $
    name
      <> " "
      <> T.pack (printf "%d%%" (windowUsedPercent window))
      <> maybe "" ((" / " <>) . formatDuration) (windowDurationSeconds window)
      <> maybe "" ((", resets in " <>) . formatDuration) (windowResetAfterSeconds window)

formatCredits :: Credits -> T.Text
formatCredits credits =
  "Credits: "
    <> (if creditsUnlimited credits then "unlimited" else fromMaybe "0" (creditsBalance credits))
    <> if creditsHasCredits credits then " available" else ""

formatDuration :: Int -> T.Text
formatDuration seconds
  | seconds < 60 = T.pack $ printf "%ds" seconds
  | seconds < 3600 = T.pack $ printf "%dm" (seconds `div` 60)
  | seconds < 86400 = T.pack $ printf "%dh" (seconds `div` 3600)
  | otherwise = T.pack $ printf "%dd" (seconds `div` 86400)
