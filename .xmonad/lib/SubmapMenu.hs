module SubmapMenu where

import Control.Monad
import System.IO
import System.Process

import XMonad

import XMonad.Util.Dzen hiding (font)

import NamedActions

-- | Like `submapName`, but also displays a list of the sub-options via dzen.
submapMenu :: String -> DzenConfig -> [((KeyMask, KeySym), NamedAction)] -> NamedAction
submapMenu name cfg keymap = addName name $ do
  (_, args) <- cfg =<< menuCfg (seconds 3, ["-l", show (length keymap)])
  -- io $ putStrLn $ unwords $ map quoteStr args
  -- io $ putStrLn $ chomp helpStr
  close <- runProcessWithInputCloseLater "dzen2" args (chomp helpStr)
  getAction $ submapName keymap
  io close
  where
    helpStr = name ++ "\n" ++ unlines (map (" "++) $ showKm keymap)
    menuCfg = timeout 0.0 >=>
              addArgs ["-e", "onstart=uncollapse"]

-- quoteStr :: String -> String
-- quoteStr s = '"' : s ++ "\""


procCfg :: FilePath -> [String] -> CreateProcess
procCfg fp a = (proc fp a)
          { std_in = CreatePipe
          , std_out = Inherit
          , std_err = Inherit
          }


-- | A version of `runProcessWithInput` that returns an IO action to close the
-- handles.
runProcessWithInputCloseLater :: MonadIO m => FilePath -> [String] -> String -> m (IO ())
runProcessWithInputCloseLater cmd args input = io $ do
  (Just pin, _, _, _) <- createProcess $ procCfg cmd args
  hPutStr pin input
  hFlush pin
  let close = hClose pin
  -- no need to waitForProcess, we ignore SIGCHLD
  return close
