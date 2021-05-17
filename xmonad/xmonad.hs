{-# LANGUAGE TupleSections, TypeSynonymInstances, MultiParamTypeClasses #-}
import Control.Monad
import Data.Maybe
import System.Environment
import System.FilePath
import System.IO
import System.Process
import Network.BSD (getHostName)

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

import System.Exit

import           NamedActions
import           SubmapMenu
import           TaffyPagerHints (pagerHints)

--------------- Config ---------------
main = do
  home <- getEnv "HOME"
  path <- fromMaybe "" <$> lookupEnv "PATH"
  setEnv "PATH" (path ++ ":" ++ (home </> "bin"))
  xmonad =<< cfg

-- Apply some transformations to the config
cfg = do
    keys <- myKeys
    return $ addKeys keys $ ewmh $ pagerHints myBaseConfig
  where
    addKeys keys c = addDescrKeys' ((modm, xK_F1), xMessage) (const keys) c

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
    , normalBorderColor = colorBg0
    , focusedBorderColor = colorBg4
    , logHook = updatePointer (0.5, 0.5) (0.25, 0.25)
    }

startup = do
  spawnOnce "mons default"
  spawn "init-xset"
  spawn "init-wallpaper"
  spawn "init-host-specific" -- This script will run any host-specific startup commands
  spawn "xmodmap ~/.Xmodmap"
  spawnOnce "compton-start"
  spawn "xrandr --dpi 96x96"
  spawn "check-dotfs"
  spawn "init-taffybars"
  spawnOnce "net-login"
  spawnOnce "login-startup"
  spawnOnce "nm-applet"

--------------- Workspaces ---------------
myWorkspaces = ["1:Web", "2:Work", "3:IRC", "4", "5", "6", "7", "8", "9"]


--------------- Theme ---------------

colorBg0 = "#282828";
colorBg1 = "#3c3836";
colorBg2 = "#504945";
colorBg3 = "#665c54";
colorBg4 = "#7c6f64";
colorFg4 = "#a89984";
colorFg3 = "#bdae93";
colorFg2 = "#d5c4a1";
colorFg1 = "#ebdbb2";
colorFg0 = "#fbf1c7";

colorGreen = "#b8bb26";
colorRed = "#fb4934";
colorYellow = "#fabd2f";
colorBlue = "#83a598";
colorOrange = "#fe8019";
colorPurple = "#d3869b";
colorAqua = "#8ec07c";

myFontName = "xft:Source Code Pro:pixelsize=14"

myTheme = defaultTheme
  { activeColor         = colorBg2
  , activeTextColor     = colorFg0
  , activeBorderColor   = colorBg2
  , inactiveColor       = colorBg0
  , inactiveTextColor   = colorFg0
  , inactiveBorderColor = colorBg0
  , urgentColor         = colorRed
  , urgentTextColor     = colorBg0
  , urgentBorderColor   = colorRed
  , fontName            = myFontName
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
  layerWindows
  . avoidStruts
  . trackFloating
  . fullscreenFull
  -- Full layout goes first in the Web workspace.
  . onWorkspace "1:Web" (lTabbedBot ||| lTabbed ||| lFull ||| lTall ||| mTall)
  -- On the Work workspace, we use the tabbed layout first.
  . onWorkspace "2:Work" (lTabbed ||| lTabbedBot ||| lTall ||| mTall ||| lFull ||| lGimp)
  -- On the IRC workspace, we can use Tall, Mirror tall, tabbed, and full.
  . onWorkspace "3:IRC" (lTall ||| mTall ||| lTabbed||| lTabbedBot  ||| lFull ||| lGimp)
  -- Workspace 4 is sometimes used for games and videos, so I use the full layout first on there.
  . onWorkspace "4" (lFull ||| lTabbed||| lTabbedBot  ||| lTall ||| mTall ||| lGimp)
  $ (lTabbed ||| lTabbedBot ||| lTall ||| mTall ||| lFull ||| lGimp)


lTabbed = noBorders $ tabbedAlways shrinkText myTheme
lTabbedBot = noBorders $ tabbedBottom shrinkText myTheme
lFull = noBorders Full
lGimp = combineTwoP (spaceLayout $ TwoPane 0.03 0.7) lTabbed lTabbed (Role "gimp-image-window")

lTall :: ModifiedLayout Spacing Tall Window
lTall = spaceLayout $ Tall 1 (3/100) (1/2)
mTall = Mirror lTall


spaceLayout :: LayoutClass l a => l a -> ModifiedLayout Spacing l a
spaceLayout = spacingRaw True tallScreenBorder True tallWindowBorder True 
screenBorderSize = 0
tallScreenBorder = Border screenBorderSize screenBorderSize screenBorderSize screenBorderSize
windowBorderSize = 16
tallWindowBorder = Border windowBorderSize windowBorderSize windowBorderSize windowBorderSize


jumpLayout :: LayoutClass l a => l a -> X ()
jumpLayout l = sendMessage $ JumpToLayout $ description l

layoutsMod = id

dmenuArgs = [ "-y", "12", "-x", "12", "-w", "1896", "-i", "-p", "$"
            --, "-h", "24" -- Requires height patch
            , "-sf", "#a7a7a7", "-nf", "#636363", "-nb", "#1e1e1e", "-sb", "#1e1e1e"
            , "-fn", "Source Code Pro-10:style=Bold"
            ]

dmenuArgStr = unwords $ map (\a -> "\"" ++ a ++ "\"") dmenuArgs

dzenCfg = DZ.font "Source Code Pro-10:style=Bold" >=>
          DZ.y 480 >=>
          DZ.onCurr (DZ.hCenter 600) >=>
          DZ.addArgs ["-bg", "#1e1e1e"] >=>
          DZ.addArgs ["-fg", "#a7a7a7"] >=>
          DZ.addArgs []

--------------- Keys ---------------

submapMenu' t = submapMenu t dzenCfg

-- This is an IO action since keys can vary between hosts
myKeys :: IO [((KeyMask, KeySym), NamedAction)]
myKeys = do
  hostname <- getHostName
  return (baseKeys ++ workspaceKeys (numberRow modm) ++ screenKeys ++ hostKeys hostname) -- `M.union` workspaceKeys

baseKeys :: [((KeyMask, KeySym), NamedAction)]
baseKeys =
  [ subtitle "window layout"
  , ((modm, xK_Return), addName "focus master" $ windows W.focusMaster)
  , ((altMask, xK_Tab), addName "next window" $ windows W.focusDown)
  , ((modm, xK_Tab), addName "prev window" $ windows W.focusUp)
  -- , ((modm, xK_w), addName "switch up" $ windows W.focusUp)

  , ((modm .|. shiftMask, xK_Return),
     addName "shift window to master" $ windows W.shiftMaster)
  , ((modm .|. shiftMask, xK_Up),
     addName "shift window down" $ windows W.swapDown)
  , ((modm .|. shiftMask, xK_Down),
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
  -- , ((modm              , xK_space),
  --     addName "prompt launch application" $ spawn ("j4-dmenu-desktop --dmenu='dmenu " ++ dmenuArgStr ++ "'"))
  --, ((modm              , xK_space), addName "run command" $ spawn ("dmenu_run " ++ dmenuArgStr))
  , ((modm              , xK_space), addName "run command" $ spawn "rofi -combi-modi window,run -show combi -modi combi")
  , ((modm .|. shiftMask, xK_space), addName "run application" $ spawn "rofi -show drun -modi drun")
  , ((modm              , xK_grave), addName "scratchpad" $ scratchpadSpawnActionCustom "alacritty --class scratchpad -e bash --login")
  , ((modm              , xK_t), addName "tmux terminal" $ spawn "alacritty -e tmux")
  , ((modm .|. shiftMask, xK_t), addName "terminal" $ spawn "alacritty -e bash --login")
  , ((modm .|. altMask, xK_t), addName "tmux attach" $ attachMenu)
  , ((modm, xK_e), submapMenu' "Applications Menu" appKeys)
  , ((modm .|. shiftMask, xK_e), submapMenu' "Remote Applications Menu" remoteAppKeys)

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

  , ((modm, xK_m), addName "mount external drive" mountMenu)

  , ((modm, xK_Escape) , addName "kill window" kill)
  , ((modm, xK_g)      , addName "toggle compositing" $ spawn "compton-toggle")

  , ((modm, xK_c), addName "swap color palette" $ spawn "colorswap")

  , ((modm, xK_v), addName "post screenshot" $ spawn "bash -c 'sleep 0.2; pscrot -s'")

  , ((modm, xK_r), submapMenu' "Misc Operations Menu" miscMenu)
  , ((modm .|. shiftMask, xK_q), addName "restart XMonad" $ restart "xmonad" True)
  ]

-- | Hostname-specific key bindings
hostKeys "homebase" =
  -- Monitor switching
  [ ((modm, xK_F2), addName "enable main monitor only" $ spawn "mons -n main")
  , ((modm, xK_F3), addName "enable second monitor only" $ spawn "mons -n second")
  , ((modm, xK_F4), addName "enable both monitors" $ spawn "mons -n both")

  -- WinVM attach/detach
  , ((modm, xK_F5), addName "detach keyboard and mouse from WinVM" (spawn "vmctx lin" >> restart "xmonad" True))
  , ((modm, xK_F6), addName "attach keyboard and mouse to WinVM" $ spawn "vmctx win")
  ]
hostKeys "nixpro" =
  [ ((modm, xK_F2), addName "disable secondary monitor" $ spawn "monitor2 off")
  , ((modm, xK_F3), addName "enable secondary monitor" $ spawn "monitor2 on")
  ]
hostKeys _ = []


appKeys :: [((KeyMask, KeySym), NamedAction)]
appKeys = map (\(key, cmd) -> (key, spawn' cmd)) appMenu

remoteAppKeys :: [((KeyMask, KeySym), NamedAction)]
remoteAppKeys = map (\(key, cmd) -> (key, spawn' ("ssh -Y forkk.ddns.net " ++ cmd))) appMenu

-- | Key bindings for application launching submap.
appMenu :: [((KeyMask, KeySym), String)]
appMenu =
  [ ((0, xK_t), "ts3client")
  , ((0, xK_v), "pavucontrol")
  , ((0, xK_e), "emacs --no-desktop")
  , ((0, xK_c), "google-chrome-stable")
  , ((0, xK_q), "quasselclient")
  , ((0, xK_s), "steam")
  , ((0, xK_d), "deluge")
  , ((0, xK_i), "google-chrome-stable --app=https://discord.com/app")
  , ((0, xK_a), "google-chrome-stable --app=https://app.element.io")
  , ((0, xK_m), "google-chrome-stable --app=https://music.youtube.com")
  , ((0, xK_n), "google-chrome-stable --app=https://netflix.com")
  , ((0, xK_p), "google-chrome-stable --app=https://plex.tv/web")
  , ((shiftMask, xK_p), "google-chrome-stable --app=https://pandora.com")
  , ((shiftMask, xK_s), "google-chrome-stable --app=https://www.sharelatex.com")
  ]


-- | Key bindings for layout switching submap.
layoutKeys :: [((KeyMask, KeySym), NamedAction)]
layoutKeys =
  [ ((0, xK_f), addName "fullscreen" $ jumpLayout lFull)
  , ((0, xK_t), addName "tabbed" $ jumpLayout lTabbed)
  , ((shiftMask, xK_t), addName "tabbed bottom" $ jumpLayout lTabbedBot)
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
screenKeys = focusKeys ++ swapKeys
  where
    focusKeys = zip (drop 2 $ asdfRow modm) (map viewScreen' [0, 1 :: Int])
    viewScreen' s = addName ("focus screen" ++ show (s+1)) $ viewScreen def $ P s
    swapKeys =
        [ ((modm .|. shiftMask, xK_d), addName "swap workspaces with next screen" swapNextScreen)
        , ((modm .|. shiftMask, xK_f), addName "swap workspaces with previus screen" swapPrevScreen)
        ]


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


-- Uses rofi to prompt the user to select an item from a list.
promptList :: MonadIO m => [String] -> m String
promptList lst = runProcessWithInput "rofi" ["-dmenu"] (unlines lst)

attachMenu :: MonadIO m => m ()
attachMenu = do
  sessList <- lines <$> runProcessWithInput "tmux" ["list-sessions", "-F", "#S"] ""
  sess <- promptList sessList
  spawn ("alacritty -e bash -c 'tmux attach -t " ++ sess ++ "'")


mountMenu :: MonadIO m => m ()
mountMenu = do
  driveList <- lines <$> runProcessWithInput "external-disks" [] ""
  drive <- promptList driveList
  spawn ("alacritty bash -c 'disksh " ++ drive ++ "'")

--------------- Manage Hook ---------------

myManageHook = composeAll . concat $
  [ [ workspaceManageHook ]

  , [ title =? "Dunst" --> manageBot ]
  , [ title =? "livewallpaper" --> manageBot ]

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
  [ propContains "quassel" className -?> doShift "3:IRC"

    -- When running via X11-Forwarding from a machine in the CS labs at school,
    -- I want gnome-terminal to be on workspace 9.
  --, (className =? "Gnome-terminal" {-<&&> propContains "cs.trinity.edu" wM_CLIENT_MACHINE-}) -?> doShift "9"
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
            io exitSuccess
        _ -> return ()
