module LGPT.Parser where

import Data.Char (isPunctuation, isSpace)

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (decimal)

import LGPT.Helpers  (Parser)
import LGPT.Numbers  (parseLonghand)
import LGPT.Request


{-
  Responsible for parsing user input into Requests that the chatbot can respond to.
  Split into sections by topic, each with a parent parser that tries each sub-parser in turn.
  If all parsers fail, an Unknown request is returned.
-}

-- Reads user input and parses it into a Request that Responder can handle.
-- Returns Unknown if no parser matches the input.
readRequest :: String -> Request
readRequest str = case parse parseRequest "<stdin>" str of
  Left  _ -> Unknown
  Right req -> req


-- Main parser that tries each category of request in order.
-- Returns the first successful match, or Unknown if nothing matches.
parseRequest :: Parser Request
parseRequest = parseGreeting
           <|> parseMaths
           <|> parseDateRequest
           <|> parseMemoryRequest
           <|> parseSetAPIKey
           <|> parseStockRequest
           <|> parseCompanyRequest
           <|> parseNewsRequest
           <|> parsePortfolioRequest


-- Parses simple greeting phrases and returns Hello.
parseGreeting :: Parser Request
parseGreeting = choice (map string' ["Hello", "Hi", "Hey", "Hello There"]) >> pure Hello


{------ Date Parsers ------}

-- Tries different date-related parsers in order.
parseDateRequest :: Parser Request
parseDateRequest = parseToday <|> parseTomorrow <|> parseDaysSince

-- Parses questions asking for today's day of the week.
parseToday :: Parser Request
parseToday = do
  choice (map string' ["What day is it?", "What's the day?", "What day is today?", "What is today?"])
  pure (DayRequest Today)

-- Parses questions asking for tomorrow's day of the week.
parseTomorrow :: Parser Request
parseTomorrow = do
  choice (map string' ["What day is it tomorrow?", "What's the day tomorrow?", "What day is tomorrow?"])
  pure (DayRequest Tomorrow)

-- Parses "how long ago" questions for a specific date in YYYY-MM-DD format.
parseDaysSince :: Parser Request
parseDaysSince = do
  choice [ string' "How long ago was"
         , string' "How many days ago was"
         , string' "When was"
         ]
  hspace
  year  <- decimal
  char '-'
  month <- decimal
  char '-'
  day   <- decimal
  optional (char '?')
  pure $ DaysSince year month day


{------ Maths Parsers ------}

-- Parses maths requests starting with "What is", "Calculate", etc.
-- Handles both normal expressions and expressions containing "that"
-- (which references the last stored result).
parseMaths :: Parser Request
parseMaths = do
  choice (map string' ["What is", "Calculate", "Compute", "Work out"])
  hspace
  that <- optional (string' "that")
  case that of
    Nothing -> Evaluate <$> parseExpr
    Just _  -> EvaluateWithResult <$> takeWhile1P Nothing (not . isPunctuation)

-- Parses a mathematical expression of the form: num op num op num...
-- Evaluates left-to-right by folding operators over the first number.
parseExpr :: Parser Expr
parseExpr = do
  num       <- parseNum
  operators <- many parseOp
  pure $ foldl (\expr op -> op expr) num operators

-- Parses a number using the longhand parser from LGPT.Numbers.
parseNum :: Parser Expr
parseNum = Number <$> parseLonghand

-- Parses an operator (+, -, *) followed by a number.
-- Returns a function to be folded into the expression.
parseOp :: Parser (Expr -> Expr)
parseOp = do
  hspace
  op <- choice [parseAdd, parseSub, parseMult]
  hspace
  op <$> parseNum
  where
    parseAdd  = choice (map string' ["plus", "added to"])       >> pure Add
    parseSub  = choice (map string' ["minus", "subtract"])      >> pure Subtract
    parseMult = choice (map string' ["times", "multiplied by"]) >> pure Multiply


{------ Memory Parsers ------}

-- Tries parsers for remembering or recalling facts.
parseMemoryRequest :: Parser Request
parseMemoryRequest = parseRemember <|> parseRecall

-- Parses "Remember that X is Y" style commands and stores the fact.
parseRemember :: Parser Request
parseRemember = do
  choice (map string' ["Remember that", "Note that", "Store that", "Save that"])
  hspace
  name  <- manyTill anySingle (string' " is ")
  hspace
  thing <- takeWhile1P Nothing (not . isPunctuation)
  pure $ Remember name thing

-- Parses "Tell me about X" style requests to recall a stored fact.
parseRecall :: Parser Request
parseRecall = do
  choice (map string' ["Tell me about", "What do you know about", "Remind me about", "Recall"])
  hspace
  name <- takeWhile1P Nothing (not . isPunctuation)
  pure $ Recall name


{------ Stock Information Parsers ------}

-- Tries parsers for stock quotes and share cost calculations.
parseStockRequest :: Parser Request
parseStockRequest = parseStockQuote <|> parseShareCost

-- Parses requests for stock details (price, change, high, low, etc.).
-- Uses parseStockDetail to decide which detail is wanted.
parseStockQuote :: Parser Request
parseStockQuote = do
  choice (map string' ["What is the stock", "What's the stock", "Get the stock", "Show the stock"])
  hspace
  detail <- parseStockDetail
  hspace
  string' "of"
  hspace
  company <- takeWhile1P Nothing (not . isPunctuation)
  optional (char '?')
  pure $ StockQuote company detail

-- Parses which stock detail is being requested.
parseStockDetail :: Parser StockDetail
parseStockDetail = choice
  [ choice (map string' ["percent change", "percentage change"]) >> pure PercentChange
  , choice (map string' ["price", "cost", "value"])              >> pure Price
  , choice (map string' ["change", "movement"])                  >> pure Change
  , choice (map string' ["high", "highest", "peak"])             >> pure High
  , choice (map string' ["low", "lowest"])                       >> pure Low
  ]

-- Parses requests for the total cost of a given number of shares.
parseShareCost :: Parser Request
parseShareCost = do
  choice (map string' ["How much are", "How much for"])
  hspace
  quantity <- decimal
  hspace
  choice (map string' ["shares", "stocks", "units"])
  hspace
  string' "of"
  hspace
  company <- takeWhile1P Nothing (not . isPunctuation)
  optional (char '?')
  pure $ ShareCost company quantity


{------ Company Information Parsers ------}

-- Tries parsers for company info and analyst opinions.
parseCompanyRequest :: Parser Request
parseCompanyRequest = parseAnalystOpinion <|> parseCompanyInfo

-- Parses requests asking for analyst consensus on a company stock.
parseAnalystOpinion :: Parser Request
parseAnalystOpinion = do
  choice (map string' ["What do analysts think of", "What do experts think of", "How do analysts rate"])
  hspace
  company <- takeWhile1P Nothing (not . isPunctuation)
  optional (char '?')
  pure $ AnalystOpinion company

-- Parses requests for general company profile information.
parseCompanyInfo :: Parser Request
parseCompanyInfo = do
  choice (map string' ["Give me information about", "Information on", "Details about"])
  hspace
  company <- takeWhile1P Nothing (not . isPunctuation)
  optional (char '?')
  pure $ CompanyInformation company


{------ News Information Parsers ------}

-- Tries parsers for news articles and "read more" requests.
parseNewsRequest :: Parser Request
parseNewsRequest = parseReadMore <|> parseNewsArticles

-- Parses requests for recent news about a company.
-- "Show me news about X (this) today/week/month"
-- Uses parseNewsDate to decide the time range (today / week / month).
parseNewsArticles :: Parser Request
parseNewsArticles = do
  choice (map string' ["Any news about", "What's the news on", "Show me news about", "Latest news on"])
  hspace
  company <- takeWhile1P Nothing (not . isSpace)
  hspace
  optional (string' "this")
  hspace
  date <- parseNewsDate
  optional (char '?')
  pure $ NewsRequest company date

-- Parses the time range for news requests (today, week, or month).
parseNewsDate :: Parser NewsDate
parseNewsDate = choice [ string' "today" >> pure NewsToday
                       , string' "week"  >> pure NewsWeek
                       , string' "month" >> pure NewsMonth
                       ]

-- Parses "read more about number X" style requests.
parseReadMore :: Parser Request
parseReadMore = do
  choice (map string' [ "I want to know more about the number"
                      , "Tell me more about number"
                      , "Read more about number"
                      , "More about article"
                      ])
  hspace
  number <- decimal
  optional (char '?')
  pure $ ReadMore number


{------ API Key Parser ------}

-- Parses commands to set the user's Finnhub API key.
parseSetAPIKey :: Parser Request
parseSetAPIKey = do
  choice (map string' ["Set API key to", "My API key is", "The API key is", "API key"])
  hspace 
  key <- takeWhile1P Nothing (not . isPunctuation)
  pure $ SetAPIKey key


{------ Portfolio Parsers ------}

-- Tries all portfolio related parsers (buy, sell, show, performance, etc.).
parsePortfolioRequest :: Parser Request
parsePortfolioRequest =
      parsePortfolioValue
  <|> parseBestStock
  <|> parseWorstStock
  <|> parsePortfolioPerformance
  <|> parseStockPerformance
  <|> parseShowPortfolio
  <|> parseBuyStock
  <|> parseSellStock

-- Parses "I bought X shares of Y" style commands.
parseBuyStock :: Parser Request
parseBuyStock = do
  choice (map string' ["I bought", "I purchased", "I acquired", "I have bought", "I've bought"])
  hspace
  quantity <- decimal
  hspace
  choice (map string' ["shares", "stocks", "units"])
  hspace
  string' "of"
  hspace
  company <- takeWhile1P Nothing (not . isPunctuation)
  optional (char '?')
  pure $ BuyStock company quantity

-- Parses "I sold X shares of Y" style commands.
parseSellStock :: Parser Request
parseSellStock = do
  choice (map string' ["I sold", "I have sold", "I've sold", "I offloaded"])
  hspace
  quantity <- decimal
  hspace
  choice (map string' ["shares", "stocks", "units"])
  hspace
  string' "of"
  hspace
  company <- takeWhile1P Nothing (not . isPunctuation)
  optional (char '?')
  pure $ SellStock company quantity

-- Parses requests to display the user's current portfolio.
parseShowPortfolio :: Parser Request
parseShowPortfolio = do
  choice (map string' ["Show me my stocks", "List my stocks", "What stocks do I own", "Show my portfolio", "List my portfolio", "Show me my portfolio"])
  optional (char '?')
  pure ShowPortfolio

-- Parses requests for overall portfolio performance.
parsePortfolioPerformance :: Parser Request
parsePortfolioPerformance = do
  choice (map string' ["How are my stocks doing", "How is my portfolio doing", "How are my shares doing", "How is my portfolio performing"])
  optional (char '?')
  pure PortfolioPerformance

-- Parses requests for performance of one specific stock in the portfolio.
parseStockPerformance :: Parser Request
parseStockPerformance = do
  choice (map string' ["How are my", "How is my"])
  hspace
  company <- takeWhile1P Nothing (not . isSpace)
  hspace
  choice (map string' ["shares doing?", "stocks doing?", "shares performing?", "stock doing?"])
  pure $ StockPerformance company

-- Parses requests for the total current value of the portfolio.
parsePortfolioValue :: Parser Request
parsePortfolioValue = do
  choice (map string' ["What is my portfolio worth?", "How much is my portfolio worth?", "What is my portfolio value?"])
  pure PortfolioValue

-- Parses requests for the best performing stock.
parseBestStock :: Parser Request
parseBestStock = do
  choice (map string' ["What is my best performing stock?", "Which is my best stock?", "What is my top performing stock?"])
  pure BestStock

-- Parses requests for the worst performing stock.
parseWorstStock :: Parser Request
parseWorstStock = do
  choice (map string' ["What is my worst performing stock?", "Which is my worst stock?", "What is my lowest performing stock?"])
  pure WorstStock