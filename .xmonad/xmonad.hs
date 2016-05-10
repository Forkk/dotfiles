{-# LANGUAGE TupleSections, TypeSynonymInstances, MultiParamTypeClasses #-}
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

import           XMonad.Actions.CycleWS
import           XMonad.Actions.OnScreen
import           XMonad.Actions.PhysicalScreens
import           XMonad.Actions.UpdatePointer
import           XMonad.Actions.Volume
import           XMonad.Actions.Submap

import           XMonad.Layout                  hiding ((|||))
import           XMonad.Layout.ComboP
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
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers
import           XMonad.Hooks.SetWMName
import           XMonad.Hooks.UrgencyHook
import           XMonad.Hooks.EwmhDesktops (ewmh)

import           XMonad.Util.EZConfig
import           XMonad.Util.Types
import           XMonad.Util.Run
import           XMonad.Util.SpawnOnce
import qualified XMonad.Util.ExtensibleState as XS
import qualified XMonad.Util.Dzen as DZ
import           XMonad.Util.Scratchpad

import qualified Graphics.X11.Xlib as X

import System.Taffybar.Hooks.PagerHints (pagerHints)
import System.Taffybar.XMonadLog (taffybarPP)
import System.Exit

import           NamedActions
import           SubmapMenu

--------------- Config ---------------
main = do
  home <- getEnv "HOME"
  path <- fromMaybe "" <$> lookupEnv "PATH"
  setEnv "PATH" (path ++ ":" ++ (home </> "bin"))
  xmonad =<< cfg

-- Apply some transformations to the config
cfg = return $ addKeys $ ewmh $ pagerHints myBaseConfig
  where
    addKeys :: XConfig l -> XConfig l
    addKeys c = addDescrKeys' ((modm, xK_F1), xMessage) (const myKeys) c

modm = mod4Mask
altMask = mod1Mask

myBaseConfig = defaultConfig
    { modMask = modm
    , terminal = "st"
    , workspaces = myWorkspaces
    , layoutHook = myLayout
    -- , keys = keys defaultConfig
    , manageHook = myManageHook <+> manageDocks
    , startupHook = startup
    , handleEventHook = docksEventHook
    , borderWidth = 4
    , normalBorderColor = "#3f3f3f"
    , focusedBorderColor = "#6f6f6f"
    , logHook = dynamicLog >> updatePointer (0.5, 0.5) (0.25, 0.25)
    }

startup = do
  mapM_ (\opts -> spawn ("xset " ++ opts)) xsetOpts
  spawnOnce "compton-start"
  spawn "init-taffybars"
  spawn "feh --bg-fill ~/wallpaper/solarized-mountains_9beat7.png"
  spawn "xrandr --dpi 96x96"
  spawn "backlight +0" -- Set backlight to the value in ~/.cache/backlight-setting
  spawn "check-dotfs"
  spawn "init-ssh-keys"
  spawnOnce "dropbox-cli start"
  spawnOnce "net-login"
  spawnOnce "login-startup"
  spawnOnce "nm-applet"
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
  . layerWindows
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
         ++ "-fn \"Source Code Pro-10:style=Bold\""

dzenCfg = DZ.font "Source Code Pro-10:style=Bold" >=>
          DZ.onCurr (DZ.hCenter 1600) >=>
          DZ.y 64 >=>
          DZ.addArgs ["-bg", "#1e1e1e"] >=>
          DZ.addArgs ["-fg", "#a7a7a7"] >=>
          DZ.addArgs []

--------------- Keys ---------------

submapMenu' t = submapMenu t dzenCfg

myKeys :: [((KeyMask, KeySym), NamedAction)]
myKeys = baseKeys ++ workspaceKeys (numberRow modm) ++ screenKeys -- `M.union` workspaceKeys

baseKeys :: [((KeyMask, KeySym), NamedAction)]
baseKeys =
  [ subtitle "window layout"
  , ((modm, xK_Return), addName "focus master" $ windows W.focusMaster)
  , ((altMask, xK_Tab), addName "next window" $ windows W.focusDown)
  , ((modm, xK_Tab), addName "prev window" $ windows W.focusUp)
  -- , ((modm, xK_w), addName "switch up" $ windows W.focusUp)

  , ((modm .|. shiftMask, xK_Return),
     addName "shift window to master" $ windows W.shiftMaster)
  , ((modm .|. shiftMask, xK_e),
     addName "shift window down" $ windows W.swapDown)
  , ((modm .|. shiftMask, xK_w),
     addName "shift window up" $ windows W.swapUp)

  , ((modm, xK_bracketleft), addName "shrink master pane" $ sendMessage Shrink)
  , ((modm, xK_bracketright), addName "expand master pane" $ sendMessage Expand)
  , ((modm, xK_apostrophe), addName "remove 1 window from master" $ sendMessage $ IncMasterN (-1))
  , ((modm, xK_semicolon), addName "add 1 window to master" $ sendMessage $ IncMasterN 1)

  , ((modm, xK_s), submapMenu' "Layout Menu" layoutKeys)

  , ((modm, xK_p), addName "tile focused window" $ withFocused $ windows . W.sink)

  , ((modm, xK_b), addName "toggle status bar" $ sendMessage ToggleStruts)

  , ((modm, xK_w), submapMenu' "Workspace Menu" $ workspaceKeys wsMenuKeys)

  -- Launch Programs
  , subtitle "launching applications"
  , ((modm              , xK_space),
      addName "prompt launch application" $ spawn ("j4-dmenu-desktop --dmenu='dmenu " ++ dmenuArgs ++ "'"))
  , ((modm .|. shiftMask, xK_space), addName "run command" $ spawn ("dmenu_run " ++ dmenuArgs))
  , ((modm              , xK_grave), addName "scratchpad" $ scratchpadSpawnActionCustom "st -c scratchpad")
  , ((modm              , xK_t), addName "tmux terminal" $ spawn "st -e tmux")
  , ((modm .|. shiftMask, xK_t), addName "terminal" $ spawn "st -e bash --login")
  , ((modm, xK_e), submapMenu' "Applications Menu" appKeys)

  -- Controls (Volume / Brightness)
  , subtitle "audio/brightness controls"
  , ((0, xF86XK_MonBrightnessUp  ), addName "increase brightness" $ changeBacklight 10)
  , ((0, xF86XK_MonBrightnessDown), addName "decrease brightness" $ changeBacklight (-10))

  , ((modm, xK_End), addName "swap audio device" $ spawn "switch-audio")
  , ((0, xF86XK_AudioRaiseVolume), addName "raise volume" $ volChange 5.0)
  , ((0, xF86XK_AudioLowerVolume), addName "lower volume" $ volChange (-5.0))
  , ((0, xF86XK_AudioMute), addName "toggle mute" toggleMuted)
  , ((modm, xK_Page_Up),   addName "raise volume" $ volChange 5.0)
  , ((modm, xK_Page_Down), addName "lower volume" $ volChange (-5.0))
  , ((modm, xK_Home), addName "toggle mute" toggleMuted)


  , subtitle "misc"
  , ((modm, xK_q), submapMenu' "Restart/Quit Menu" quitKeys)

  , ((modm, xK_l), addName "lock screen" $ spawn "lock-screen")

  , ((modm, xK_Escape) , addName "kill window" kill)
  , ((modm, xK_g)      , addName "toggle compositing" $ spawn "compton-toggle")


  , ((modm, xK_F2), addName "disable secondary monitor" $ spawn "monitor2 off")
  , ((modm, xK_F3), addName "enable secondary monitor" $ spawn "monitor2 on")

  , ((modm, xK_c), addName "swap color palette" $ spawn "colorswap")

  , ((modm, xK_r), submapMenu' "Misc Operations Menu" miscMenu)
  ]


-- | Key bindings for application launching submap.
appKeys :: [((KeyMask, KeySym), NamedAction)]
appKeys =
  [ ((0, xK_t), addName "teamspeak" $ spawn "ts3client")
  , ((0, xK_v), spawn' "pavucontrol")
  , ((0, xK_e), addName "emacs" $ spawn "emacs --no-desktop")
  , ((0, xK_c), spawn' "chromium")
  , ((0, xK_q), spawn' "quasselclient")
  , ((0, xK_s), spawn' "steam")
  , ((0, xK_d), spawn' "deluge")
  ]


-- | Key bindings for layout switching submap.
layoutKeys :: [((KeyMask, KeySym), NamedAction)]
layoutKeys =
  [ ((0, xK_f), addName "fullscreen" $ jumpLayout lFull)
  , ((0, xK_t), addName "tabbed" $ jumpLayout lTabbed)
  , ((0, xK_v), addName "tall vertical" $ jumpLayout lTall)
  , ((0, xK_h), addName "tall horizontal" $ jumpLayout mTall)
  , ((0, xK_g), addName "gimp layout" $ jumpLayout lGimp)
  ]


-- | Key bindings for quitting/restarting XMonad.
quitKeys :: [((KeyMask, KeySym), NamedAction)]
quitKeys =
  [ ((0, xK_r), addName "restart XMonad" $ restart "xmonad" True)
  , ((shiftMask, xK_r), addName "restart without saving state" $ restart "xmonad" False)
  , ((modm .|. shiftMask, xK_q), addName "log out" $ io exitSuccess)
  , ((0, xK_o), addName "temporarily switch to Openbox" $ restart "temp-openbox" True)
  ]


-- | Key bindings for miscellaneous actions like switching the WM name (for
-- swing apps).
miscMenu :: [((KeyMask, KeySym), NamedAction)]
miscMenu =
  [ ((0, xK_n), addName "set WM name to LG3D to make Swing apps work" $ setWMName "LG3D")
  , ((shiftMask, xK_n), addName "set WM name back to XMonad" $ setWMName "XMonad")

  , ((0, xK_r), oneName (refresh, "refresh"))
  ]


-- | Keybinds for switching workspaces and moving windows between workspaces.
workspaceKeys :: [(KeyMask, KeySym)] -> [((KeyMask, KeySym), NamedAction)]
workspaceKeys keys = concat
  [ zip viewKeys (map viewWS myWorkspaces)
  , zip shiftKeys (map shiftTo myWorkspaces)
  ]
  where
    viewKeys = keys
    shiftKeys = map (\(m, k) -> (m .|. shiftMask, k)) keys
    viewWS ws = addName ("view workspace" ++ ws) $ windows $ view ws
    shiftTo ws = addName ("shift to workspace" ++ ws) $ windows $ shift ws


-- | Keybinds for switching screens.
screenKeys :: [((KeyMask, KeySym), NamedAction)]
screenKeys = zip (drop 2 $ asdfRow modm) (map viewScreen' [0, 1 :: Int])
  where
    viewScreen' s = addName ("focus screen" ++ show (s+1)) $ viewScreen $ P s


asdfRow :: KeyMask -> [(KeyMask, KeySym)]
asdfRow mask = map (mask,) [ xK_a, xK_s, xK_d, xK_f, xK_g, xK_h, xK_j, xK_k, xK_l ]

qwertyRow :: KeyMask -> [(KeyMask, KeySym)]
qwertyRow mask = map (mask,) [ xK_q, xK_w, xK_e, xK_r, xK_t, xK_y, xK_u, xK_i, xK_o, xK_p ]

zxcvRow :: KeyMask -> [(KeyMask, KeySym)]
zxcvRow mask = map (mask,) [ xK_z, xK_x, xK_c, xK_v, xK_b, xK_n, xK_m ]

wsMenuKeys :: [(KeyMask, KeySym)]
wsMenuKeys = take 4 (numberRow 0) ++ take 5 (qwertyRow 0)

-- | A key range containing all of the number row keys from 1 to 9 and 0.
numberRow :: KeyMask -> [(KeyMask, KeySym)]
numberRow mask = map (mask,) ([xK_1..xK_9] ++ [xK_0])

-- | A key range containing all of the function keys from 1 to 12.
fnKeys :: KeyMask -> [(KeyMask, KeySym)]
fnKeys mask = map (mask,) [xK_F1..xK_F12]


--------------- Actions ---------------

toggleMuted :: MonadIO m => m ()
toggleMuted = spawn "volume toggle notify"

volChange :: MonadIO m => Double -> m ()
volChange by = do
  when (by > 0) $ spawn ("volume +" ++ show (floor by) ++ " notify")
  when (by < 0) $ spawn ("volume -" ++ show (-floor by) ++ " notify")


changeBacklight :: MonadIO m => Double -> m ()
changeBacklight by = do
  when (by > 0) $ spawn ("backlight +" ++ show (floor by) ++ " notify")
  when (by < 0) $ spawn ("backlight -" ++ show (-floor by) ++ " notify")


--------------- Manage Hook ---------------

myManageHook = composeAll . concat $
  [ [ workspaceManageHook ]

  , [ className =? "Xfce4-notifyd" --> manageTop ]

  , [ (className =? "Firefox" <&&> resource =? "Dialog") --> doFloat ]

  , [ className =? c --> doFloat | c <- floatClassMatches ]
  , [ fmap (c `isInfixOf`) className --> doFloat | c <- floatClassContains ]

  , [className =? "Friends" --> doFloat]
    -- Handle Steam's nonsense.
  , map (className =? "Steam" -->)
    [ title =? "Friends" --> doFloat
    , fmap ("Update News" `isInfixOf`) title --> doFloat
    ]
  , [ scratchpadManageHook $ W.RationalRect 0.125 0.0 0.75 0.75 ]
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

--------------- Notification Handling ---------------

-- | Keeps a list of unmanaged windows which should always be on top.
data LayerWins = LayerWins { lwTop :: [Window], lwBot :: [Window] }
    deriving (Read, Show, Typeable)

instance ExtensionClass LayerWins where
    initialValue = LayerWins [] []
    -- extensionType = PersistentExtension

manageTop :: ManageHook
manageTop = do
  w <- ask
  liftX $ reveal w
  liftX $ XS.modify (addWin w)
  doF (W.delete w)
  where
    addWin :: Window -> LayerWins -> LayerWins
    addWin w tw = tw { lwTop = w : lwTop tw }

manageBot :: ManageHook
manageBot = do
  w <- ask
  liftX $ reveal w
  liftX $ XS.modify (addWin w)
  doF (W.delete w)
  where
    addWin :: Window -> LayerWins -> LayerWins
    addWin w tw = tw { lwBot = w : lwBot tw }


data LayerWinsMod a = LayerWinsMod
    deriving (Read, Show, Typeable)

instance LayoutModifier LayerWinsMod Window where
  hook _ = do
    (LayerWins top bot) <- XS.get
    withDisplay $ \d -> liftIO $ do
      mapM_ (raiseWindow d) top
      mapM_ (lowerWindow d) bot

layerWindows :: l a -> ModifiedLayout LayerWinsMod l a
layerWindows = ModifiedLayout LayerWinsMod


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
