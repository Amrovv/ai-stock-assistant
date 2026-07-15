{-# LANGUAGE DeriveGeneric #-}
-- Deriving Generic allows automatic JSON encoding/decoding with aeson


module LGPT.Memory where

import System.Directory            (doesFileExist, createDirectoryIfMissing)
import System.FilePath             (takeDirectory)
import Data.Aeson                  (ToJSON, FromJSON, encode, decode)
import qualified Data.ByteString.Lazy as BL
import GHC.Generics                (Generic)
import qualified Data.Map          as M

import LGPT.API                    (Article)

{-
  Defines the Memory type used as the chatbot's state throughout a session.
  Also handles saving and loading memory to/from disk as JSON.
-}

{------ Data Types ------}

-- Chatbot's memory, stored as a state throughout the session.
-- Using a record syntax for easy access to different fields during the session.
data Memory = Memory 
    { lastResult     :: Maybe String  -- Result of last evaluated expression.
    , portfolio      :: Portfolio     -- User's stock porfolio.
    , articles       :: Articles      -- Most recently requested news articles.
    , facts          :: Facts         -- Facts named by the user.
    , apiKey         :: Maybe String  -- The user's API key for finnhub
    } deriving (Generic)


-- A single stock entry in the user's portfolio
data PortfolioEntry = PortfolioEntry
    { company   :: String
    , ticker    :: String
    , numShares :: Integer
    , buyPrice  :: Double 
    } deriving (Show, Generic)

-- Portfolio is a mapping from company name to portfolio entry.
type Portfolio = M.Map String PortfolioEntry

-- Articles is a mapping from article number to article details.
type Articles  = M.Map Integer Article

-- Facts is a mapping from fact name to fact details.
type Facts     = M.Map String String

{------ JSON Instances ------}

instance ToJSON Memory
instance FromJSON Memory
instance ToJSON PortfolioEntry
instance FromJSON PortfolioEntry


{------ Memory Management ------}


-- Empty memory used when no saved memory found on disk.
emptyMemory :: Memory
emptyMemory = Memory
    { lastResult   = Nothing
    , portfolio    = M.empty
    , articles     = M.empty
    , facts        = M.empty
    , apiKey       = Nothing
    }


-- Saves the full memory state to disk as JSON.
saveMemory :: Memory -> IO ()
saveMemory memory = do
    let filePath = "data/memory.json"
    createDirectoryIfMissing True (takeDirectory filePath)
    BL.writeFile filePath (encode memory)


-- Loads memory from disk. Returns Nothing if no saved state exists.
loadMemory :: IO (Maybe Memory)
loadMemory = do
    let filePath = "data/memory.json"
    exists <- doesFileExist filePath
    if not exists
        then return Nothing
        else decode <$> BL.readFile filePath

