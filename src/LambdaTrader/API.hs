{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
-- Deriving Generic allows automatic JSON encoding/decoding with Aeson

module LambdaTrader.API where

import Network.HTTP.Request
import Data.Aeson              (FromJSON(..), ToJSON, eitherDecode, withObject, (.:))
import System.Environment      (getEnv)
import Data.List               (sortBy)
import Data.Ord                (comparing, Down(..))
import GHC.Generics            (Generic)


{-
  Handles all communication with the Finnhub API.
  Each function builds the appropriate URL and decodes the JSON response.
  All calls go through apiCall, which handles the API key and decoding.
-}


-- Checks for the API key, appends it, makes an API call to Finnhub and decodes the JSON response.
-- Returns an error message if no API key is set or if decoding fails.
apiCall :: FromJSON a => Maybe String -> String -> IO (Either String a)
apiCall apiKey url = do 
    case apiKey of
        Nothing  -> pure $ Left "You have not provided an API key"
        Just key -> do
                    let url' = url ++ key
                    result <- get url'
                    pure $ eitherDecode (responseBody result)


-- Fetches the current price, change, percent change, high and low for a stock.
getStockQuote :: Maybe String -> String -> IO (Either String Quote)
getStockQuote key symbol = apiCall key $ "https://finnhub.io/api/v1/quote?symbol=" ++ symbol ++ "&token="


-- Searches for a company by name and returns its first matching ticker.
-- Returns an error if no matching company is found.
getCompanyTicker :: Maybe String -> String -> IO (Either String Ticker)    
getCompanyTicker key company = do 
    tickersf <- apiCall key $ "https://finnhub.io/api/v1/search?q=" ++ company ++ "&token="
    case tickersf of 
        Left err                    -> pure $ Left err
        Right (TickerLookup 0 [])      -> pure $ Left "Company not found"
        Right (TickerLookup _ (x:_)) -> pure $ Right x
       

-- Fetches general company profile information (name, industry, country, etc.).
getCompanyInfo :: Maybe String -> String -> IO (Either String CompanyInfo)    
getCompanyInfo key company = apiCall key $ "https://finnhub.io/api/v1/stock/profile2?symbol=" ++ company ++ "&token="

-- Fetches analyst recommendation data (strong buy, buy, hold, sell, strong sell)
getAnalystOpinion :: Maybe String -> String -> IO (Either String [AnalystRecommendation])    
getAnalystOpinion key company = apiCall key $ "https://finnhub.io/api/v1/stock/recommendation?symbol=" ++ company ++ "&token="

-- Fetches recent news articles for a company within a given date range.
-- Returns up to 15 articles sorted by date (newest first).
getNewsArticles :: String -> String -> Maybe String -> String -> IO (Either String [Article])  
getNewsArticles from to key company = do 
    result <- apiCall key $ "https://finnhub.io/api/v1/company-news?symbol=" ++ company ++ "&from=" ++ from ++ "&to=" ++ to ++ "&token="
    case result of 
        Left err  -> pure $ Left err
        Right articles -> pure $ Right (take 15 $ sortBy (comparing (Down . datetime)) articles )

{------ Data Types for API responses ------}

-- Represents a single news article returned by Finnhub.

data Article = Article 
    { datetime :: Int
    , headline :: String
    , source   :: String
    , summary  :: String
    , url      :: String
    } deriving (Show, Generic)

instance ToJSON Article
instance FromJSON Article where
    parseJSON = withObject "Article" $ \v ->
        Article 
            <$> v .: "datetime"
            <*> v .: "headline"
            <*> v .: "source"
            <*> v .: "summary"
            <*> v .: "url"


-- Contains counts of analyst buy/sell recommendations.
data AnalystRecommendation = AnalystRecommendation
    { strongBuy  :: Int
    , buy        :: Int
    , hold       :: Int
    , sell       :: Int
    , strongSell :: Int
    } deriving (Show)

instance FromJSON AnalystRecommendation where
    parseJSON = withObject "AnalystRecommendation" $ \v ->
        AnalystRecommendation 
            <$> v .: "strongBuy"
            <*> v .: "buy"
            <*> v .: "hold"
            <*> v .: "sell"
            <*> v .: "strongSell"


-- Company profile information.
data CompanyInfo = CompanyInfo
    { name      :: String
    , country   :: String
    , currency  :: String
    , marketCap :: Double
    , industry  :: String
    , ipo       :: String
    , website   :: String          
    } deriving (Show)

instance FromJSON CompanyInfo where
    parseJSON = withObject "CompanyInfo" $ \v ->
        CompanyInfo
            <$> v .: "name"
            <*> v .: "country"
            <*> v .: "currency"
            <*> v .: "marketCapitalization"
            <*> v .: "finnhubIndustry"
            <*> v .: "ipo"
            <*> v .: "weburl"


-- Wrapper for company ticker search results.
data TickerLookup = TickerLookup
    { count   :: Int
    , tickers  :: [Ticker]     
    } deriving (Show)

instance FromJSON TickerLookup where
    parseJSON = withObject "TickerLookup" $ \v ->
        TickerLookup
            <$> v .: "count"
            <*> v .: "result"  

-- Company ticker symbol.
newtype Ticker = Ticker 
    { symbol :: String
    } deriving (Show)

instance FromJSON Ticker where
    parseJSON = withObject "Ticker" $ \v ->
        Ticker <$> v .: "symbol"

-- Current stock quote data.
data Quote = Quote
  { currentPrice  :: Double
  , change        :: Double
  , percentChange :: Double
  , high          :: Double
  , low           :: Double
  } deriving (Eq, Ord, Show)

instance FromJSON Quote where
  parseJSON = withObject "StockQuote" $ \v ->
    Quote
      <$> v .: "c"   -- currentPrice
      <*> v .: "d"   -- change
      <*> v .: "dp"  -- percentChange
      <*> v .: "h"   -- high
      <*> v .: "l"   -- low




