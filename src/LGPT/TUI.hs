module LGPT.TUI where

{-
This file is the main entry point to your coursework.
You can create or modify any files in src/ as much as you like.
-}

import Control.Monad       (void, forever)
import Control.Monad.State (StateT, lift, runStateT)

import LGPT.Helpers        (prompt)
import LGPT.Memory         (Memory, emptyMemory, loadMemory, saveMemory)
import LGPT.Parser         (readRequest)
import LGPT.Responder      (respondTo)

{-
  Entry point for the chatbot.
  Starts the REPL, loading any previously saved memory from disk.
-}

-- Loads saved memory (if any) and starts the chatbot REPL.
runREPL :: IO ()
runREPL = do
  maybeMem <- loadMemory
  case maybeMem of
    Nothing -> void $ runStateT runMemory emptyMemory
    Just memory -> void $ runStateT runMemory memory

    
-- Main REPL loop. Runs forever, reading input, parsing it,
-- and passing the resulting Request to the responder.
runMemory :: StateT Memory IO ()
runMemory = forever $ do
  lift $ putStr prompt
  req <- lift getLine
  respondTo (readRequest req)


