module LambdaTrader.Request where

{-
  Contains all data types shared between Parser.hs and Responder.hs.
  The main type is Request, which defines every kind of input the chatbot can process.
-}

-- Relative day used in DayRequest.
-- Avoids pattern matching on a string.
data RelativeDay = Today | Tomorrow
  deriving (Eq, Ord, Show)

-- The specific stock detail that is being requestd.
-- Avoid pattern matching on a string.
data StockDetail = Price 
                 | Change 
                 | PercentChange 
                 | High 
                 | Low
  deriving (Eq, Ord, Show)

-- The main Request type.
-- Each constructor represents a different request the chatbot can respond to.
-- Constructors with fields carry information extracted from the user's input.
data Request  = Hello 
              | Unknown 
              | DayRequest           RelativeDay
              | DaysSince            Integer Int Int 
              | Evaluate             Expr
              | EvaluateWithResult   String
              | Remember             String String 
              | Recall               String 
              | StockQuote           String StockDetail
              | ShareCost            String Integer
              | CompanyInformation   String
              | AnalystOpinion        String
              | NewsRequest          String NewsDate
              | ReadMore             Integer
              | BuyStock             String Integer
              | SellStock            String Integer
              | ShowPortfolio
              | PortfolioPerformance 
              | PortfolioValue
              | BestStock
              | WorstStock
              | StockPerformance   String
              | SetAPIKey          String
              deriving (Eq, Ord, Show)

-- Represents simple arithmetic expressions for Evaluate requests 
data Expr = Add      Expr Expr 
          | Subtract Expr Expr
          | Multiply Expr Expr
          | Number   Int
          deriving (Eq, Ord, Show)

-- Time frame for news requests
-- Avoid pattern matching on a string.
data NewsDate = NewsToday | NewsWeek | NewsMonth
  deriving (Eq, Ord, Show)

