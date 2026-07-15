module LambdaTrader.Responder where


import Control.Monad       (foldM)
import Control.Monad.State (StateT, get, modify, lift)
import Data.Time           (getZonedTime, zonedTimeToLocalTime, localDay,
                           dayOfWeek, addDays, fromGregorian, diffDays)
import Data.Maybe          (catMaybes)
import Data.List           (maximumBy, minimumBy)
import Data.Function       (on)
import qualified Data.Map  as M
import Text.Printf         (printf)

import LambdaTrader.Request
import LambdaTrader.Memory
import LambdaTrader.API
import LambdaTrader.Numbers        (printLonghand)
import LambdaTrader.Parser         (readRequest)

{- 
  This module handles the chatbot's responses to parsed user Requests.
  It uses StateT Memory IO () to manage state memory while performing IO
  operations such as printing replies and interacting with the Finnhub API.

  The core function is repondTo which pattern-matches on each Request type and
  produces the appropriate chain of operations and response.
  All helper functions are grouped at the bottom.
-}

-- Pattern matches on a Request and produces the appropriate response.
-- Uses StateT to access and modify the chatbot's memory.
respondTo :: Request -> StateT Memory IO ()

respondTo Hello = lift $ putStrLn "Hi there!"

{------ Date Responders ------}

-- Returns today's day of the week using Data.Time.
respondTo (DayRequest Today) = do
  time <- lift getZonedTime
  let day = dayOfWeek . localDay . zonedTimeToLocalTime $ time
  lift . putStrLn $ "Today is " ++ show day ++ "."

-- Returns tomorrow's day of the week using Data.Time.
respondTo (DayRequest Tomorrow) = do
  time <- lift getZonedTime
  let today    = localDay . zonedTimeToLocalTime $ time
  let tomorrow = dayOfWeek $ addDays 1 today
  lift . putStrLn $ "Tomorrow is " ++ show tomorrow ++ "."

-- Calculates and returns the number of days since a given date.
respondTo (DaysSince y m d) = do
  time <- lift getZonedTime
  let targetDay  = fromGregorian y m d
  let today      = localDay . zonedTimeToLocalTime $ time
  let difference = diffDays today targetDay
  lift . putStrLn $ show targetDay ++ " was " ++ show difference ++ " days ago."

{------ Maths Responders ------}

-- Evaluates an arithmetic expression and stores the result in memory
-- as a longhand string for later use with "that".
respondTo (Evaluate expr) = do
  let result = eval expr
  modify (\mem -> mem { lastResult = Just (printLonghand result) })
  lift $ putStrLn ("The answer is " ++ printLonghand result ++ ".")

-- Evaluates an expression that contains "that" by replacing it with the
-- last stored result, then re-parses and responds to the new request.
respondTo (EvaluateWithResult rest) = do
  memory <- get
  case lastResult memory of
    Nothing -> lift $ putStrLn "I haven't evaluated anything yet."
    Just n  -> do
      let expRest = unwords . map (\x -> if x == "that" then n else x) . words $ rest
      respondTo (readRequest ("What is " ++ n ++ " " ++ expRest ++ "?"))

{------ Memory Responders ------}

-- Stores a named fact in memory and saves the updated memory to disk.
respondTo (Remember name thing) = do
  modify (\mem -> mem { facts = M.insert name thing (facts mem) })
  memory <- get
  lift $ saveMemory memory
  lift $ putStrLn "Okay."

-- Looks up and returns a named fact from memory.
respondTo (Recall name) = do
  memory <- get
  case M.lookup name (facts memory) of
    Nothing -> lift . putStrLn $ "Sorry, I don't know anything about " ++ name ++ "."
    Just f  -> lift . putStrLn $ "Sure - " ++ name ++ " is " ++ f ++ "."


-- Unknow request response
respondTo Unknown = lift $ putStrLn "I don't understand"

{------ API Key Responder ------}

-- Saves the user's Finnhub API key to memory and writes it to disk.
respondTo (SetAPIKey key) = do
  modify (\mem -> mem { apiKey = Just key })
  memory <- get
  lift $ saveMemory memory
  lift $ putStrLn "API key saved."

{------ Stock Information Responders ------}

-- Returns the requested stock detail (price, change, etc.) for a company.
respondTo (StockQuote company detail) = do
  onTicker company $ \t -> 
      onQuote (symbol t) $ \quote -> 
        lift . putStrLn $ showQuote detail quote company 

-- Calculates and returns the total cost of a given number of shares.
respondTo (ShareCost company quantity) = do
  onTicker company $ \t -> do
      onQuote (symbol t) $ \quote ->
        lift . putStrLn $ "The price of " ++ show quantity ++ " shares of " ++ company ++
          " is $" ++ (printf "%.2f" (sharesCost (currentPrice quote) quantity) :: String)

{------ Company Information Responders ------}

-- Returns basic profile information for a company (industry, country, etc.).
respondTo (CompanyInformation company) = do
  onTicker company $ \t -> do
      memory <- get
      let key = apiKey memory
      information <- lift $ getCompanyInfo key (symbol t)
      case information of 
        Left err   -> lift . putStrLn $ "Error fetching company information: " ++ err
        Right info -> lift . putStrLn $
          name info ++ " is a " ++ industry info ++ " company based in " ++ country info ++ ".\n" ++
          "Market Cap: " ++ show (marketCap info) ++ "\n" ++
          "Currency: " ++ currency info ++ "\n" ++
          "IPO: " ++ ipo info ++ "\n" ++
          "Website: " ++ website info

-- Returns a natural language summary of analyst consensus for a stock.
-- Calculates a weighted score from recommendation counts, then converts it
-- to a readable description.
respondTo (AnalystOpinion company) = do
  onTicker company $ \t -> do
      memory <- get
      let key = apiKey memory
      analystOpinion <- lift $ getAnalystOpinion key (symbol t)
      case analystOpinion of
        Left err    -> lift . putStrLn $ "Error fetching analyst recommendation: " ++ err
        Right []    -> lift $ putStrLn "No analyst data available."
        Right (d:_) -> lift . putStrLn $ analystConsensus . analystScore $ d

{------ News Information Responders ------}

-- Fetches recent news articles for a company and stores them in memory
-- so the user can request more details by number.
-- Maps each artice to a line in a numbered list.
respondTo (NewsRequest company date) = do
  onTicker company $ \t -> do
      memory <- get
      let key = apiKey memory
      (fromDate, toDate) <- lift $ newsDateRange date
      articles <- lift $ getNewsArticles fromDate toDate key (symbol t)
      case articles of
        Left err -> lift . putStrLn $ "Error fetching news articles: " ++ err
        Right a  -> do
          let articleMap = M.fromList $ zip [1..15] a
          modify (\mem -> mem { articles = articleMap })
          lift $ putStrLn $ "\nLatest news for " ++ company ++ " (" ++ symbol t ++ "):\n"
          mapM_ (\(i, art) ->
            lift $ putStrLn (show i ++ ". " ++ headline art ++ " By: " ++ source art)
            ) (M.toList articleMap)
          lift $ putStrLn "\nReply with 'I want to know more about the number (1-15)' to read more details."

-- Displays the full details of a news article previously fetched by NewsRequest.
respondTo (ReadMore number)
  | number < 1 || number > 15 = lift . putStrLn $ "Not a valid number, must be 1-15"
  | otherwise = do
    memory <- get
    let articleMap = articles memory
    if M.null articleMap
      then lift . putStrLn $ "You haven't asked me for any news articles yet"
      else case M.lookup number articleMap of
             Nothing       -> lift . putStrLn $ "No such article exists"
             Just article' -> lift $ putStrLn $ formatArticle article'

{------ Portfolio Responders ------}

-- Updates the portfolio to reflect buying additional shares of a company.
-- using a weighted average buy price.
respondTo (BuyStock company quantity) = do
  onTicker company $ \t -> do
      memory <- get
      let port = portfolio memory
      onQuote (symbol t) $ \quote -> do
          let newEntry   = PortfolioEntry { company = company, ticker = symbol t, numShares = quantity, buyPrice = currentPrice quote }
              finalEntry = case M.lookup company port of
                Nothing       -> newEntry
                Just oldEntry -> combineEntries oldEntry newEntry
              updatedPort = M.insert company finalEntry port
          modify (\mem -> mem { portfolio = updatedPort })
          updatedMemory <- get
          lift $ saveMemory updatedMemory
          lift . putStrLn $ "Got it! You now own " ++ show (numShares finalEntry) ++ " stocks of " ++ company ++ " at an average price of $" ++ printf "%.2f" (buyPrice finalEntry)

-- Updates the portfolio to reflect selling shares of a company.
-- Shows an error if the user tries to sell more shares than they own.
respondTo (SellStock company quantity) = do
  memory <- get
  let port = portfolio memory
  case M.lookup company port of
    Nothing    -> lift . putStrLn $ "You do not own any shares of " ++ company
    Just entry -> do
      let ownedShares = numShares entry
      if ownedShares < quantity then
        lift . putStrLn $ "You only own " ++ show ownedShares ++ " shares of " ++ company ++ ". Try again."
      else if ownedShares == quantity then do
        let updatedPort = M.delete company port
        modify (\mem -> mem { portfolio = updatedPort })
        updatedMemory <- get
        lift $ saveMemory updatedMemory
        lift . putStrLn $ "All shares of " ++ company ++ " sold!"
      else do
        let updatedPort = M.adjust (updateShares (ownedShares - quantity)) company port
        modify (\mem -> mem { portfolio = updatedPort })
        updatedMemory <- get
        lift $ saveMemory updatedMemory
        lift . putStrLn $ "Got it! You now own " ++ show (ownedShares - quantity) ++ " shares of " ++ company ++ "."

-- Displays the user's portfolio with current prices and total values.
-- Maps each Entry to a nicely formatted string.
respondTo ShowPortfolio = do
  memory <- get
  let entries = M.elems (portfolio memory)
  lift $ putStrLn "\nHere is your Portfolio:\n"
  mapM_ (\e -> do
    onQuote (ticker e) $ \quote -> 
      lift . putStrLn $ company e ++ " | Shares: " ++ show (numShares e) ++ " | Current Price: $" ++ printf "%.2f" (currentPrice quote) ++ " | Total Value: $" ++ printf "%.2f" (fromIntegral (numShares e) * currentPrice quote)
    ) entries
  lift $ putStrLn "\nReply with 'How are my stocks doing?' to get an analysis on your portfolio."

-- Displays performance metrics (buy price, % change, profit/loss) for each stock.
-- Maps each Entry to a nicely formatted string.
respondTo PortfolioPerformance = do
  memory <- get
  let entries = M.elems (portfolio memory)
  lift $ putStrLn "\nHere is your Portfolio Performance:\n"
  mapM_ (\e -> do
    onQuote (ticker e) $ \quote -> 
      lift . putStrLn $ company e ++ " | Buy Price: $" ++ printf "%.2f" (buyPrice e) ++ " | Current Price: $" ++ printf "%.2f" (currentPrice quote) ++ " | Percentage Change: " ++ showChange (percentDiff (buyPrice e) (currentPrice quote)) ++ " | Total Profit / Loss: " ++ profitLoss e (currentPrice quote)
    ) entries
  lift $ putStrLn "\nReply with 'What is my portfolio worth?' to see your total value."

-- Calculates and displays the total current value and overall profit/loss of the portfolio.
-- Folds over the list of Portfolio Entries to accumulate total portfolio value and total profic / loss.
respondTo PortfolioValue = do
  memory <- get
  let key = apiKey memory
  let entries = M.elems (portfolio memory)
  currentTotal <- foldM (\acc e -> do
    quoteResult <- lift $ getStockQuote key (ticker e)
    case quoteResult of
      Left _      -> pure acc
      Right quote -> pure $ acc + fromIntegral (numShares e) * currentPrice quote
    ) 0.0 entries
  let boughtTotal = foldr (\e acc -> acc + fromIntegral (numShares e) * buyPrice e) 0.0 entries
  let change  = currentTotal - boughtTotal
  let prefix  = if change >= 0 then "+$" else "-$"
  lift . putStrLn $ "Total portfolio value: $" ++ printf "%.2f" currentTotal
  lift . putStrLn $ "Total profit / loss: "    ++ prefix ++ printf "%.2f" (abs change)

-- Displays the best performing stock in the portfolio.
respondTo BestStock  = extremeStock (maximumBy (compare `on` snd)) "best"  "return"

-- Displays the worst performing stock in the portfolio.
respondTo WorstStock = extremeStock (minimumBy (compare `on` snd)) "worst" "loss"

-- Displays performance metrics for a single stock in the portfolio.
respondTo (StockPerformance companyName) = do
  memory <- get
  case M.lookup companyName (portfolio memory) of
    Nothing -> lift . putStrLn $ "You do not own any shares of " ++ companyName
    Just e  -> do
      lift . putStrLn $ "\nHere is your " ++ companyName ++ " stock performance:\n"
      onQuote (ticker e) $ \quote ->
        lift . putStrLn $ company e ++ " | Buy Price: $" ++ printf "%.2f" (buyPrice e) ++ " | Current Price: $" ++ printf "%.2f" (currentPrice quote) ++ " | Percentage Change: " ++ showChange (percentDiff (buyPrice e) (currentPrice quote)) ++ " | Total Profit / Loss: " ++ profitLoss e (currentPrice quote)


{------ Helper Function ------}

-- Looks up a company's ticker symbol, then runs the given action with it.
-- Prints an error if the ticker lookup fails.
onTicker :: String -> (Ticker -> StateT Memory IO ()) -> StateT Memory IO ()
onTicker company action = do
  memory <- get
  let key = apiKey memory
  ticker <- lift $ getCompanyTicker key company
  case ticker of
    Left err -> lift . putStrLn $ "Error looking up ticker: " ++ err
    Right t  -> action t

-- Looks up a stock quote by ticker symbol, then runs the given action with it.
-- Prints an error if the quote lookup fails.
onQuote :: String -> (Quote -> StateT Memory IO ()) -> StateT Memory IO ()
onQuote ticker action = do
  memory <- get
  let key = apiKey memory
  result <- lift $ getStockQuote key ticker
  case result of
    Left err    -> lift . putStrLn $ "Error fetching stock: " ++ err
    Right quote -> action quote

-- Builds a list of (PortfolioEntry, percentageChange) pairs.
-- Uses Maybe to safely handle any failed quote lookups.
performanceList :: [PortfolioEntry] -> StateT Memory IO [Maybe (PortfolioEntry, Double)]
performanceList entries = do
  memory <- get
  let key = apiKey memory
  mapM (\e -> do
    quoteResult <- lift $ getStockQuote key (ticker e)
    case quoteResult of
        Left _  -> pure Nothing 
        Right q -> pure $ Just (e, percentDiff (buyPrice e) (currentPrice q))
    ) entries

-- Finds and displays either the best or worst performing stock.
-- Uses the supplied selecting function (maximumBy or minimumBy).
extremeStock :: ([(PortfolioEntry, Double)] -> (PortfolioEntry, Double)) 
             -> String -> String -> StateT Memory IO ()
extremeStock selecting label result = do
  memory <- get
  let entries = M.elems (portfolio memory)
  results <- performanceList entries
  let validResults = catMaybes results
  case validResults of
    [] -> lift $ putStrLn "You do not own any stocks."
    _  -> do
      let stock = selecting validResults
      lift . putStrLn $ "Your " ++ label ++ " performing stock is " ++ company (fst stock)
                     ++ " with a " ++ result ++ " of " ++ showChange (snd stock)

-- Calculates the percentage change between buy price and current price.
percentDiff :: Double -> Double -> Double
percentDiff buyPrice currentPrice = 
    (currentPrice - buyPrice) / buyPrice * 100

-- Formats a percentage change with an up/down arrow.
showChange :: Double -> String
showChange change
    | change > 0  = "↑ " ++ show change ++ "%"
    | change == 0 = show change ++ "%"
    | otherwise   = "↓ " ++ show change ++ "%"

-- Calculates and formats the total profit or loss for one portfolio entry.
profitLoss :: PortfolioEntry -> Double -> String
profitLoss entry currentPrice = 
  let sharesTotal  = sharesCost (buyPrice entry) (numShares entry)
      currentTotal = sharesCost currentPrice (numShares entry)
      change       = currentTotal - sharesTotal
  in if change >= 0 then "+$" ++ printf "%.2f" change
     else "-$" ++ printf "%.2f" change

-- Updates the number of shares in a PortfolioEntry.
updateShares :: Integer -> PortfolioEntry -> PortfolioEntry
updateShares newShares entry = entry { numShares = newShares }

-- Calculates the new weighted average buy price after adding more shares.
newAvgPrice :: Integer -> Double -> Integer -> Double -> Double
newAvgPrice oldQuantity oldPrice newQuantity newPrice = 
    (fromIntegral oldQuantity * oldPrice + fromIntegral newQuantity * newPrice) 
    / fromIntegral (oldQuantity + newQuantity)

-- Combines a new purchase with an existing entry using weighted average price.
combineEntries :: PortfolioEntry -> PortfolioEntry -> PortfolioEntry
combineEntries oldEntry newEntry = newEntry 
  { numShares = numShares newEntry + numShares oldEntry
  , buyPrice  = newAvgPrice (numShares oldEntry) (buyPrice oldEntry) 
                            (numShares newEntry) (buyPrice newEntry) 
  }

-- Formats a news article nicely for display.
formatArticle :: Article -> String
formatArticle a = unlines
  [ ""
  , headline a
  , "Source: " ++ source a
  , ""
  , summary a
  , ""
  , "Read more: " ++ url a
  ]

-- Calculates the from/to date range for news requests based on NewsDate.
newsDateRange :: NewsDate -> IO (String, String)
newsDateRange date = do
  time <- getZonedTime
  let toDate   = localDay . zonedTimeToLocalTime $ time
  let fromDate = addDays ((-1) * dateOffset date) toDate
  pure (show fromDate, show toDate)

-- Converts a NewsDate into the number of days to look back.
dateOffset :: NewsDate -> Integer
dateOffset NewsToday = 0
dateOffset NewsWeek  = 7
dateOffset NewsMonth = 30

-- Formats a stock quote response depending on the requested StockDetail.
showQuote :: StockDetail -> Quote -> String -> String
showQuote Price         q company = "The stock price of "    ++ company ++ " is $" ++ show (currentPrice q)
showQuote Change        q company = "The change of "         ++ company ++ " is $" ++ show (change q)
showQuote PercentChange q company = "The percent change of " ++ company ++ " is "  ++ show (percentChange q) ++ "%"
showQuote High          q company = "The day high of "       ++ company ++ " is $" ++ show (high q)
showQuote Low           q company = "The day low of "        ++ company ++ " is $" ++ show (low q)

-- Calculates a weighted analyst score from recommendation counts.
-- Lower score = more positive sentiment.
analystScore :: AnalystRecommendation -> Double
analystScore r = fromIntegral (strongBuy r * 1 + buy r * 2 + hold r * 3 + sell r * 4 + strongSell r * 5)
               / fromIntegral (strongBuy r + buy r + hold r + sell r + strongSell r)

-- Converts an analyst score into a natural language description.
analystConsensus :: Double -> String
analystConsensus score 
  | score <= 1.5                = "Strong analyst interest with significant buying activity"
  | score > 1.5 && score <= 2.5 = "Analysts are generally positive, think it's worth buying"
  | score > 2.5 && score <= 3.5 = "Analysts are mixed, no clear consensus"
  | score > 3.5 && score <= 4.5 = "Slight lean towards selling, not many recommending a buy"
  | otherwise                   = "Strong negative sentiment, most analysts think it's a sell"

-- Recursively evaluates an arithmetic expression tree.
eval :: Expr -> Int
eval (Add a b)      = eval b + eval a
eval (Subtract a b) = eval b - eval a
eval (Multiply a b) = eval b * eval a
eval (Number n)     = n

-- Calculates the total cost of a given quantity of shares at a price.
sharesCost :: Double -> Integer -> Double
sharesCost price quantity = price * fromIntegral quantity