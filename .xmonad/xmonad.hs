import           XMonad

import qualified Data.Map                       as M
import Data.List
import           Graphics.X11.ExtraTypes.XF86

import qualified XMonad.StackSet                as W

import           XMonad.Actions.CycleWS
import           XMonad.Actions.OnScreen
import           XMonad.Actions.PhysicalScreens
import           XMonad.Layout
import           XMonad.Layout.Fullscreen
import           XMonad.Layout.NoBorders
import           XMonad.Layout.PerWorkspace
import           XMonad.Layout.Tabbed
import           XMonad.Operations
import           XMonad.Prompt
import           XMonad.Prompt.Input

import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers
import           XMonad.Hooks.SetWMName
import           XMonad.Hooks.UrgencyHook
import           XMonad.Hooks.EwmhDesktops (ewmh)
import           XMonad.Util.EZConfig
import           XMonad.Util.Types

import System.Taffybar.Hooks.PagerHints (pagerHints)
import System.Taffybar.XMonadLog (taffybarPP)
import System.Exit

--------------- Config ---------------
main = xmonad =<< cfg

-- Apply some transformations to the config
cfg = return $ ewmh $ pagerHints myBaseConfig

modm = mod4Mask
altMask = mod1Mask

myBaseConfig = defaultConfig
    { modMask = modm
    , terminal = "urxvt"
    , workspaces = myWorkspaces
    , layoutHook = myLayout
    , keys = \c -> mkKeymap c myKeys `M.union` M.fromList workspaceKeys
    , manageHook = myManageHook <+> manageDocks
    , startupHook = startup
    , handleEventHook = docksEventHook
    }

startup = do
    spawn "killall taffybar-linux-x86_64"
    spawn "taffybar"

--------------- Workspaces ---------------
myWorkspaces = ["1:Web", "2:Work", "3:IRC", "4", "5", "6", "7", "8", "9"]


--------------- Layout ---------------
myLayout =
      avoidStruts
    . noBorders
    . fullscreenFull
    -- Full layout goes first in the Web workspace.
    . onWorkspace "1:Web" (Full ||| lTabbed ||| lTall ||| mTall)
    -- On the Work workspace, we use the tabbed layout first.
    . onWorkspace "2:Work" (lTabbed ||| lTall ||| mTall ||| Full)
    -- On the IRC workspace, we can use Tall, Mirror tall, tabbed, and full.
    . onWorkspace "3:IRC" (lTall ||| mTall ||| lTabbed ||| Full)
    -- Workspace 4 is sometimes used for games and videos, so I use the full layout first on there.
    . onWorkspace "4" (Full ||| lTabbed ||| lTall ||| mTall)
    $ (lTabbed ||| lTall ||| mTall ||| Full)
    where lTall     = Tall 1 (3/100) (1/2)
          mTall     = Mirror lTall
          lTabbed   = simpleTabbed

layoutsMod = id


--------------- Keys ---------------
myKeys = concat [
    [ ("M-r", refresh)

    -- Layout Hotkeys
    , ("M-<Return>", windows W.focusMaster)
    , ("M-e",    windows W.focusDown      )
    , ("M-w",    windows W.focusUp        )

    , ("M-S-<Return>", windows W.swapMaster)
    , ("M-S-e",        windows W.swapDown  )
    , ("M-S-w",        windows W.swapUp    )

    , ("M-z",    sendMessage FirstLayout)
    , ("M-x",    sendMessage NextLayout)

    , ("M-p",    withFocused $ windows . W.sink)

    , ("M-b",    sendMessage $ ToggleStruts)


    -- Controls (Volume / Brightness)
    , ("<XF86MonBrightnessUp>",     spawn "xbacklight -inc 5")
    , ("<XF86MonBrightnessDown>",   spawn "xbacklight -dec 5")

    -- Launch Programs
    , ("M-<Space>", spawn "dmenu_run -l 10")
    , ("M-t"      , spawn "urxvt")
    , ("M-v"      , spawn "pavucontrol")
    , ("M-l"      , spawn "lock-screen")
    , ("M-i"      , spawn "chromium")
    , ("M-C-f"    , spawn "firefox")
    , ("M-C-e"    , spawn "emacs --no-desktop")
    , ("M-o"      , spawn "emacsclient -c")
    , ("M-C-q"    , spawn "quasselclient")
    , ("M-C-t"    , spawn "ts3client")

    , ("M-q", restart "xmonad" True)
    , ("M-C-S-q", quitPrompt)
    , ("M-C-S-o", restart "temp-openbox" True)

    -- Misc
    , ("M-<Esc>"  , kill)
    , ("M-c"      , spawn "toggle-cursor")
    , ("M-g"      , spawn "toggle-compton")

    -- Hacks
    , ("M-S-j", setWMName "LG3D")
    , ("M-S-k", setWMName "XMonad")

    -- Enable/disable 2nd montior.
    , ("M-S-C-<F1>", spawn "monitor2 off")
    , ("M-S-C-<F2>", spawn "monitor2 on")
    ]
    -- Switching Workspaces
    , [("M-S-" ++ show i,  windows $ W.shift wspc) | (wspc, i) <- zip myWorkspaces [1..9 :: Integer]]
    , [("M-C-" ++ show i, (windows $ W.shift wspc) >> (windows $ viewOnScreen 0 wspc)) | (wspc, i) <- zip myWorkspaces [1..9 :: Integer]]
    -- Switching Screens
    , [("M-<F" ++ show i ++ ">",   viewScreen $ P (i-1)) | i <- [1..3 :: Int]]
    , [("M-S-<F" ++ show i ++ ">", sendToScreen $ P (i-1)) | i <- [1..3 :: Int]]
    , [("M-C-<F" ++ show i ++ ">", sendToScreen (P (i-1)) >> viewScreen (P i)) | i <- [1..3 :: Int]]
    ]


workspaceKeys =
    [ ((m .|. modm, k), windows (f i))
      | (i, k) <- zip myWorkspaces ([xK_1 .. xK_9] ++ [xK_0])
      , (f, m) <- [ (viewOnScreen 0, 0)
                  , (viewOnScreen 1, altMask)
                  , (W.greedyView, altMask .|. controlMask) ]
    ]


--------------- Manage Hook ---------------

myManageHook = composeAll $
    [ workspaceManageHook

    , (className =? "Firefox" <&&> resource =? "Dialog") --> doFloat
    ]

-- Manage hooks for moving windows to their proper default workspaces.
workspaceManageHook = composeOne
    -- Quassel goes in the IRC workspace.
    [ (propContains "quassel" className) -?> doShift "3:IRC"

    -- Firefox windows go in workspace 1.
    , (className =? "Firefox") -?> doShift "1:Web"
    ]

-- A Query which returns true if the given property contains the given string.
propContains :: String -> Query String -> Query Bool
propContains str prop = fmap (str `isInfixOf`) prop


--------------- Custom Actions ---------------

-- | Run a function that modifies the stack on the current screen. Like
-- `onScreen`, but passes in the current screen ID.
onCurrentScreen :: (ScreenId -> arg -> WindowSet -> WindowSet)
                -> arg -> WindowSet -> WindowSet
onCurrentScreen f arg st = f cur arg st
  where
    cur = W.screen $ W.current st


-- | Opens a prompt where the user must type "quit" to close XMonad. This
-- function will call `exitWith ExitSuccess` if the user types "quit" into the
-- prompt.
quitPrompt :: X ()
quitPrompt = do
    responseM <- inputPrompt defaultXPConfig "Please type \"quit\" and press enter to log out."
    case responseM of
        Just "quit" ->
            io (exitWith ExitSuccess)
        _ -> return ()
