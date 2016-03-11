{-# LANGUAGE TupleSections #-}
import Control.Monad
import Data.Maybe
import System.Environment
import System.FilePath
import System.IO
import System.Process

import           XMonad hiding ((|||))

import qualified Data.Map                       as M
import Data.List
import           Graphics.X11.ExtraTypes.XF86

import qualified XMonad.StackSet                as W
import           XMonad.StackSet (view, shift)

import           XMonad.Layout.ComboP
import           XMonad.Actions.CycleWS
import           XMonad.Actions.OnScreen
import           XMonad.Actions.PhysicalScreens
import           XMonad.Actions.UpdatePointer
import           XMonad.Actions.Volume
import           XMonad.Layout                  hiding ((|||))
import           XMonad.Layout.Fullscreen
import           XMonad.Layout.NoBorders
import           XMonad.Layout.PerWorkspace
import           XMonad.Layout.TrackFloating
import           XMonad.Layout.TwoPane
import           XMonad.Layout.SimpleDecoration
import           XMonad.Layout.Decoration
import           XMonad.Layout.Tabbed
import           XMonad.Layout.Spacing
import           XMonad.Layout.IM
import           XMonad.Layout.ResizableTile
import           XMonad.Layout.LayoutCombinators
import           XMonad.Layout.WindowNavigation
import           XMonad.Operations
import           XMonad.Prompt
import           XMonad.Prompt.Input
import           XMonad.Util.Run

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
main = do
  home <- getEnv "HOME"
  path <- fromMaybe "" <$> lookupEnv "PATH"
  setEnv "PATH" (path ++ ":" ++ (home </> "bin"))
  xmonad =<< cfg

-- Apply some transformations to the config
cfg = return $ ewmh $ pagerHints myBaseConfig

modm = mod4Mask
altMask = mod1Mask

myBaseConfig = defaultConfig
    { modMask = modm
    , terminal = "st"
    , workspaces = myWorkspaces
    , layoutHook = myLayout
    , keys = \c -> myKeys c
    , manageHook = myManageHook <+> manageDocks
    , startupHook = startup
    , handleEventHook = docksEventHook
    , borderWidth = 4
    , normalBorderColor = "#3f3f3f"
    , focusedBorderColor = "#6f6f6f"
    , logHook = dynamicLog >> updatePointer (0.5, 0.5) (0.25, 0.25)
    }

startup = do
  spawn "taffybar"
  spawn "killall taffybar-linux-x86_64"
  spawn "compton-start"
  mapM_ (\opts -> spawn ("xset " ++ opts)) xsetOpts
  spawn "feh --bg-fill ~/wallpaper/solarized-mountains_9beat7.png"
  spawn "xrandr --dpi 96x96"
  spawn "net-login"
  spawn "login-startup"
  spawn "check-dotfs"
  where
    xsetOpts =
      [ "-b" -- Disable bell.
      , "-dpms" -- Don't turn off my screen.
      , "s off" -- Disable screensaver.
      , "r rate 200 42" -- Key repeat delay and rate.
      ]

--------------- Workspaces ---------------
myWorkspaces = ["1:Web", "2:Work", "3:IRC", "4", "5", "6", "7", "8", "9"]


--------------- Theme ---------------

myFontName = "xft:Source Code Pro:pixelsize=14"

myTheme = defaultTheme
  { activeColor         = "#2f2f2f"
  , inactiveColor       = "#1f1f1f"
  , urgentColor         = "#ffff00"
  , activeBorderColor   = "#3f3f3f"
  , inactiveBorderColor = "#1f1f1f"
  , urgentBorderColor   = "##00ff00"
  , activeTextColor     = "#f0f0f0"
  , inactiveTextColor   = "#f0f0f0"
  , urgentTextColor     = "#fff0f0"
  , fontName            = myFontName
  -- , fontName            = "-monospace-*-*-*-*-*-10-*-*-*-*-*-*-*"
  , decoWidth           = 200
  , decoHeight          = 20
  , windowTitleAddons   = []
  , windowTitleIcons    = []
  }

themedXPConfig = defaultXPConfig
  { font = myFontName
  , fgColor = "#f0f0f0"
  , bgColor = "#1f1f1f"
  , fgHLight = "#f0f0f0"
  , bgHLight = "#2f2f2f"
  , borderColor = "#f0f0f0"
  , promptBorderWidth = 0
  }

--------------- Layout ---------------
myLayout =
  id
  . avoidStruts
  . trackFloating
  . fullscreenFull
  -- Full layout goes first in the Web workspace.
  . onWorkspace "1:Web" (lFull ||| lTabbed ||| lTall ||| mTall)
  -- On the Work workspace, we use the tabbed layout first.
  . onWorkspace "2:Work" (lTabbed ||| lTall ||| mTall ||| lFull ||| lGimp)
  -- On the IRC workspace, we can use Tall, Mirror tall, tabbed, and full.
  . onWorkspace "3:IRC" (lTall ||| mTall ||| lTabbed ||| lFull ||| lGimp)
  -- Workspace 4 is sometimes used for games and videos, so I use the full layout first on there.
  . onWorkspace "4" (lFull ||| lTabbed ||| lTall ||| mTall ||| lGimp)
  $ (lTabbed ||| lTall ||| mTall ||| lFull ||| lGimp)


lTabbed = noBorders $ tabbedAlways shrinkText myTheme
lTall = spacing 8 $ Tall 1 (3/100) (1/2)
mTall = Mirror lTall
lFull = noBorders Full
lGimp = combineTwoP (spacing 8 $ TwoPane 0.03 0.7) lTabbed lTabbed (Role "gimp-image-window")


jumpLayout :: LayoutClass l a => l a -> X ()
jumpLayout l = sendMessage $ JumpToLayout $ description l

layoutsMod = id

dmenuArgs = "-y 12 -x 12 -h 24 -w 1896 -i -q -p \"$\" "
         ++ "-sf \"#a7a7a7\" -nf \"#636363\" -nb \"#1e1e1e\" -sb \"#1e1e1e\" "
         ++ "-fn \"Source Code Pro-9:style=Bold\""


--------------- Keys ---------------
baseKeys :: XConfig l -> M.Map (KeyMask, KeySym) (X ())
baseKeys c = mkKeymap c
  [ ("M-r", refresh)

  -- Layout Hotkeys
  , ("M-<Return>", windows W.focusMaster)
  , ("M-e",    windows W.focusDown      )
  , ("M-w",    windows W.focusUp        )

  , ("M-S-<Return>", windows W.shiftMaster)
  , ("M-S-e",        windows W.swapDown  )
  , ("M-S-w",        windows W.swapUp    )

  , ("M-[", sendMessage Shrink)
  , ("M-]", sendMessage Expand)
  , ("M-'", sendMessage $ IncMasterN (-1))
  , ("M-;", sendMessage $ IncMasterN 1)

  , ("M-a", jumpLayout lFull)
  , ("M-s", jumpLayout lTabbed)
  , ("M-x", jumpLayout lTall)
  , ("M-z", jumpLayout mTall)
  , ("M-M1-g", jumpLayout lGimp)

  , ("M-p",    withFocused $ windows . W.sink)

  , ("M-b",    sendMessage $ ToggleStruts)


  , ("M-C-<Right>",   sendMessage $ Move R)
  , ("M-C-<Left>",    sendMessage $ Move L)


  -- Controls (Volume / Brightness)
  , ("<XF86MonBrightnessUp>",     changeBacklight 5)
  , ("<XF86MonBrightnessDown>",   changeBacklight (-5))
  -- , ("<XF86MonBrightnessUp>",     spawn "xbacklight -inc 5")
  -- , ("<XF86MonBrightnessDown>",   spawn "xbacklight -dec 5")

  , ("M-<End>", spawn "switch-audio")
  , ("<XF86AudioRaiseVolume>", volChange 5.0)
  , ("<XF86AudioLowerVolume>", volChange (-5.0))
  , ("<XF86AudioMute>", toggleMuted)
  , ("M-<Page_Up>", volChange 5.0)
  , ("M-<Page_Down>", volChange (-5.0))
  , ("M-<Home>", toggleMuted)

  -- Launch Programs
  , ("M-<Space>", spawn ("dmenu_run " ++ dmenuArgs))
  , ("M-t"      , spawn "st -e tmux")
  , ("M-S-t"    , spawn "st")
  , ("M-v"      , spawn "pavucontrol")
  , ("M-l"      , spawn "lock-screen")
  , ("M-i"      , spawn "chromium")
  , ("M-C-e"    , spawn "emacs --no-desktop")
  , ("M-C-q"    , spawn "quasselclient")
  , ("M-C-t"    , spawn "ts3client")

  , ("M-q", restart "xmonad" True)
  , ("M-S-q", restart "xmonad" False)
  , ("M-C-S-q", quitPrompt)
  , ("M-C-S-o", restart "temp-openbox" True)

  -- Misc
  , ("M-<Esc>"  , kill)
  , ("M-c"      , spawn "toggle-cursor")
  , ("M-g"      , spawn "compton-toggle")

  -- Hacks
  , ("M-S-j", setWMName "LG3D")
  , ("M-S-k", setWMName "XMonad")

  -- Enable/disable 2nd montior.
  , ("M-S-C-<F1>", spawn "monitor2 off")
  , ("M-S-C-<F2>", spawn "monitor2 on")
  ]

toggleMuted :: MonadIO m => m ()
toggleMuted = spawn "volume toggle notify"

volChange :: MonadIO m => Double -> m ()
volChange by = do
  when (by > 0) $ spawn ("volume +" ++ show (floor by) ++ " notify")
  when (by < 0) $ spawn ("volume -" ++ show (-floor by) ++ " notify")


changeBacklight :: MonadIO m => Double -> m ()
changeBacklight by =
  spawn ("xbacklight -steps 1 -inc " ++ show by ++ "; \
        \ update-notify.sh backlight \"Brightness Changed\" \
        \ -i notification-display-brightness \
        \ -h int:value:$(printf \"%.0f\" $(xbacklight -get))")

myKeys :: XConfig l -> M.Map (KeyMask, KeySym) (X ())
myKeys c = baseKeys c `M.union` workspaceScreenKeys-- `M.union` workspaceKeys

workspaceScreenKeys :: M.Map (KeyMask, KeySym) (X ())
workspaceScreenKeys = foldr M.union M.empty
  [ keyRange (numberRow modm) (map (windows . view) myWorkspaces)
  , keyRange (numberRow (modm .|. shiftMask)) (map (windows . shift) myWorkspaces)
  , keyRange (drop 2 $ asdfRow modm) (map (viewScreen . P) [0, 1 :: Int])
  ]


  -- -- Switching Workspaces
  -- , [("M-S-" ++ show i,  windows $ W.shift wspc) | (wspc, i) <- zip myWorkspaces [1..9 :: Integer]]

  -- , [("M-C-" ++ show i, (windows $ W.shift wspc) >> (windows $ viewOnScreen 0 wspc)) | (wspc, i) <- zip myWorkspaces [1..9 :: Integer]]
  -- -- Switching Screens
  -- , [("M-<F" ++ show i ++ ">",   viewScreen $ P (i-1)) | i <- [1..3 :: Int]]
  -- , [("M-S-<F" ++ show i ++ ">", sendToScreen $ P (i-1)) | i <- [1..3 :: Int]]
  -- , [("M-C-<F" ++ show i ++ ">", sendToScreen (P (i-1)) >> viewScreen (P i)) | i <- [1..3 :: Int]]
  -- ]

asdfRow :: KeyMask -> [(KeyMask, KeySym)]
asdfRow mask = map (mask,) [ xK_a, xK_s, xK_d, xK_f, xK_g, xK_h, xK_j, xK_k, xK_l ]

qwertyRow :: KeyMask -> [(KeyMask, KeySym)]
qwertyRow mask = map (mask,) [ xK_q, xK_w, xK_e, xK_r, xK_t, xK_y, xK_u, xK_i, xK_o, xK_p ]

zxcvRow :: KeyMask -> [(KeyMask, KeySym)]
zxcvRow mask = map (mask,) [ xK_z, xK_x, xK_c, xK_v, xK_b, xK_n, xK_m ]

-- | A key range containing all of the number row keys from 1 to 9 and 0.
numberRow :: KeyMask -> [(KeyMask, KeySym)]
numberRow mask = map (mask,) ([xK_1..xK_9] ++ [xK_0])

-- | A key range containing all of the function keys from 1 to 9 and 0.
fnKeys :: KeyMask -> [(KeyMask, KeySym)]
fnKeys mask = map (mask,) [xK_F1..xK_F12]

-- | Takes a range of keys and zips them with the given list of actions to
-- produce a key map.
--
-- If one of the lists is shorter, the other will be truncated.
keyRange :: [(KeyMask, KeySym)] -> [X ()] -> M.Map (KeyMask, KeySym) (X ())
keyRange keys actions = M.fromList $ zip keys actions


-- workspaceKeys ""
workspaceKeys =
    [ ((m .|. modm, k), windows (f i))
      | (i, k) <- zip myWorkspaces ([xK_1 .. xK_9] ++ [xK_0])
      , (f, m) <- [ (viewOnScreen 0, 0)
                  , (viewOnScreen 1, altMask)
                  , (W.greedyView, altMask .|. controlMask) ]
    ]


--------------- Manage Hook ---------------

myManageHook = composeAll . concat $
  [ [ workspaceManageHook ]

  , [ className =? "Xfce4-notifyd" --> doIgnore ]

  , [ (className =? "Firefox" <&&> resource =? "Dialog") --> doFloat ]

  , [ className =? c --> doFloat | c <- floatClassMatches ]
  , [ fmap (c `isInfixOf`) className --> doFloat | c <- floatClassContains ]

  , [className =? "Friends" --> doFloat]
    -- Handle Steam's nonsense.
  , map (className =? "Steam" -->)
    [ title =? "Friends" --> doFloat
    , fmap ("Update News" `isInfixOf`) title --> doFloat
    ]
  ]
  where
    -- Float windows with these classes.
    floatClassMatches = ["Xmessage", "Gxmessage"]
    -- Float windows whose class contains any of these strings.
    floatClassContains = []

-- Manage hooks for moving windows to their proper default workspaces.
workspaceManageHook = composeOne
  -- Quassel goes in the IRC workspace.
  [ (propContains "quassel" className) -?> doShift "3:IRC"

    -- When running via X11-Forwarding from a machine in the CS labs at school,
    -- I want gnome-terminal to be on workspace 9.
  , (className =? "Gnome-terminal" {-<&&> propContains "cs.trinity.edu" wM_CLIENT_MACHINE-}) -?> doShift "9"
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
    responseM <- inputPrompt themedXPConfig "Please type \"quit\" and press enter to log out."
    case responseM of
        Just "quit" ->
            io (exitWith ExitSuccess)
        _ -> return ()
