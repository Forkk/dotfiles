module SubmapMenu where

import Codec.Binary.UTF8.String
import Control.Monad
import System.IO
import System.Process (runInteractiveProcess)

import XMonad

import XMonad.Util.Dzen hiding (font)

import NamedActions

-- | Like `submapName`, but also displays a list of the sub-options via dzen.
submapMenu :: String -> DzenConfig -> [((KeyMask, KeySym), NamedAction)] -> NamedAction
submapMenu name cfg keys = addName name $ do
  (_, args) <- cfg =<< menuCfg (seconds 3, ["-l", show (length keys)])
  close <- runProcessWithInputCloseLater "dzen2" args (chomp helpStr)
  getAction $ submapName keys
  -- spawn "xmessage worked"
  io close
  where
    helpStr = name ++ "\n" ++ unlines (map (" "++) $ showKm keys)
    menuCfg = timeout 0.0 >=>
              addArgs ["-e", "onstart=uncollapse"]


-- | A version of `runProcessWithInput` that returns an IO action to close the
-- handles.
runProcessWithInputCloseLater :: MonadIO m => FilePath -> [String] -> String -> m (IO ())
runProcessWithInputCloseLater cmd args input = io $ do
  (pin, pout, perr, _) <- runInteractiveProcess (encodeString cmd)
                                            (map encodeString args) Nothing Nothing
  hPutStr pin input
  hFlush pin
  let close = do
        hClose pin
        hClose pout
        hClose perr
  -- no need to waitForProcess, we ignore SIGCHLD
  return close
