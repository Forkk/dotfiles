import Control.Applicative hiding ((<|>), optional, many)
import Control.Exception hiding (try)
import Control.Monad
import Control.Monad.IO.Class
import Data.Maybe
import qualified Data.Text as T

import System.Taffybar
import System.Taffybar.SimpleConfig
import System.Taffybar.Widget
import System.Taffybar.Widget.Generic.PollingBar
import System.Taffybar.Widget.Generic.PollingLabel
import System.Taffybar.Widget.Generic.PollingGraph

import System.Taffybar.Information.Memory
import System.Taffybar.Information.CPU
import System.Taffybar.Information.CPU2

import System.Taffybar.Context (TaffyIO)

import System.Environment

import qualified Data.Map as M
import Data.List
import GI.Gtk hiding (init, main)
import qualified GI.Gtk as G
import System.Process
import Text.Read
import Text.Printf
import Text.Parsec
import Text.Parsec.String
import Network.HostName

colorGreen = "#b8bb26"
fcolorGreen = (0.722, 0.733, 0.149, 1.0)
colorRed = "#fb4934"
fcolorRed = (0.984, 0.286, 0.204, 1.0)
colorYellow = "fabd2f"
fcolorYellow = (0.98, 0.741, 0.184, 1.0)
colorBlue = "#83a598"
fcolorBlue = (0.514, 0.647, 0.596, 1.0)
colorOrange = "#fe8019"

graphBGColor = (0.114, 0.125, 0.129, 1.0)


formatNetworkInfo :: String -> String
formatNetworkInfo str = "<span color='" ++ colorYellow ++ "'>" ++ str ++ "</span>"

memCallback = do
  mi <- parseMeminfo
  return [memoryUsedRatio mi]

-- An IO action that checks the temperature of a device.
type TempCallback = IO (Maybe Integer)

tempFileCbk :: String -> TempCallback
tempFileCbk file = do
  str <- readFile file
  return ((`quot` 1000) <$> readMaybe str)

tempGpuCbk :: TempCallback
tempGpuCbk = readMaybe . dropLast2 <$> readCreateProcess (shell "nvidia-smi | grep '[0-9]\\+C' | awk '{ print $3; }' | head -n 1") ""
    where dropLast2 lst@(a:b:_) = init $ init lst
          dropLast2 _ = ""

tempGraphCbk :: TempCallback -> IO [Double]
tempGraphCbk cbk = maybe [0.0] ((: []) . toPercent) <$> cbk
    where
      rangeMin = 30.0
      rangeMax = 100.0
      toPercent temp = (fromIntegral temp - rangeMin) / (rangeMax - rangeMin)

stripEnd :: Char -> String -> String
stripEnd ch [] = []
stripEnd ch str
    | [ch] `isSuffixOf` str = stripEnd ch $ init str
    | otherwise = str

stripBegin :: Char -> String -> String
stripBegin ch [] = []
stripBegin ch str
    | [ch] `isPrefixOf` str = stripBegin ch $ tail str
    | otherwise = str

catchAny :: IO a -> (SomeException -> IO a) -> IO a
catchAny = Control.Exception.catch

maybePoll :: IO String -> IO String
maybePoll func = catchAny func (\err -> print err >> return "-")

tempMonitorNew :: Maybe String -> TempCallback -> TaffyIO G.Widget
tempMonitorNew name tcbk = do
  label <- pollingLabelNew 0.5 cbk
  graph <- pollingGraphNew tempGraphCfg 0.5 $ tempGraphCbk tcbk
  box <- boxNew OrientationHorizontal 1
  case name of
    Just n -> do
      lbl <- labelNew $ Just $ T.pack n
      boxPackStart box lbl False False 8
    Nothing -> return ()
  boxPackStart box label False False 0
  boxPackEnd box graph False False 0
  widgetShowAll box
  toWidget box
  where
    cbk = do
      temp <- tcbk
      case temp of
          Just t -> do
              return $ T.pack $ printf "%02d°C" t
          Nothing -> do return $ T.pack "<span fgcolor='red'>ERR</span>"


-- | Creates a horizontal box containing several widgets
packBox :: MonadIO m => [m Widget] -> m Widget
packBox mkContents = do
  box <- G.boxNew G.OrientationHorizontal 0
  contents <- sequence mkContents
  mapM_ (containerAdd box) contents
  widgetShow box
  toWidget box


-- | Creates a horizontal box containing a single widget and the given CSS class
classBox :: MonadIO m => String -> Widget -> m Widget
classBox cls content = packClassBox cls [return content]

-- | Creates a horizontal box containing several widgets with the given CSS class
packClassBox :: MonadIO m => String -> [m Widget] -> m Widget
packClassBox cls mkContents = do
    box <- packBox mkContents
    _ <- widgetSetClassGI box $ T.pack cls
    return box


diskIO "homebase" = packClassBox "io" [hddMainIO, hddD0IO, hddD1IO, hddAuxIO]
  where
    hddMainIO = dioMonitorNew (hddIOGraphCfg "main") 0.5 "sdb"
    hddD0IO = dioMonitorNew (hddIOGraphCfg "data0") 0.5 "sdc"
    hddD1IO = dioMonitorNew (hddIOGraphCfg "data1") 0.5 "sdd"
    hddAuxIO = dioMonitorNew (hddIOGraphCfg "aux") 0.5 "sde"
diskIO "ultpro" = packClassBox "io" [hddRootIO, hddHomeIO]
  where
    hddRootIO = dioMonitorNew (hddIOGraphCfg "root") 0.5 "sdb"
    hddHomeIO = dioMonitorNew (hddIOGraphCfg "home") 0.5 "sda"
diskIO _ = packClassBox "io" []

cpuWidget hostname = packClassBox "cpu" [cpu, cpuTemp]
  where
    cpu = cpuMonitorNew cpuGraphCfg 0.5 "cpu"
    cpuTemp = tempMonitorNew Nothing (tempFileCbk $ sensorFile hostname)
    sensorFile "homebase" = "/sys/bus/platform/devices/coretemp.0/hwmon/hwmon0/temp1_input"
    sensorFile "ultpro" = "/sys/bus/platform/devices/coretemp.0/hwmon/hwmon1/temp1_input"
    sensorFile _ = "/dev/null"

gpuWidget = packClassBox "gpu" [gpuLabel, gpuTemp]
  where
    gpuLabel = liftIO (toWidget =<< labelNew (Just $ T.pack "gpu"))
    gpuTemp = tempMonitorNew (Just "gpu") tempGpuCbk

clock =
    classBox "clock" =<< textClockNew Nothing "%a %b %_d %H:%M" 1

main = do
  hostname <- getHostName
  let workspaces = workspacesNew $ defaultWorkspacesConfig {
        maxIcons = Just 0
      }

      mem = classBox "memory" =<< pollingGraphNew memGraphCfg 1 memCallback
      net = classBox "network" =<< networkMonitorNew defaultNetFormat Nothing
      -- FIXME: According to the name of this function, clearly this is the best way to do this.
      tray = sniTrayThatStartsWatcherEvenThoughThisIsABadWayToDoIt
      maybeGPU = [gpuWidget | hostname == "homebase"]
  simpleTaffybar defaultSimpleTaffyConfig
       { startWidgets = [ workspaces ]
       , endWidgets = reverse ([ net, cpuWidget hostname, mem ] ++ maybeGPU
                            ++ [ diskIO hostname, tray, clock ])
       , barPosition = Bottom
       , barHeight = 24
       , widgetSpacing = 0
       , cssPath = Just "/home/forkk/taffybar/taffybar.css"
       }


cpuGraphCfg = defaultGraphConfig
    { graphDataColors = [ fcolorGreen, fcolorBlue ]
    , graphLabel = Just $ T.pack "cpu"
    , graphBackgroundColor = graphBGColor
    , graphBorderColor = graphBGColor
    }

tempGraphCfg = defaultGraphConfig
    { graphDataColors = [ fcolorYellow ]
    , graphBackgroundColor = graphBGColor
    , graphBorderColor = graphBGColor
    }

memGraphCfg = defaultGraphConfig
    { graphDataColors = [ fcolorRed ]
    , graphLabel = Just $ T.pack "mem"
    , graphBackgroundColor = graphBGColor
    , graphBorderColor = graphBGColor
    }

hddIOGraphCfg sname = defaultGraphConfig
    { graphDataColors = [ fcolorGreen, fcolorRed ]
    , graphLabel = Just name
    , graphBackgroundColor = graphBGColor
    , graphBorderColor = graphBGColor
    }
    where name = T.pack sname

netGraphConfig = defaultGraphConfig
    { graphDataColors = [ fcolorGreen, fcolorRed ]
    , graphLabel = Just $ T.pack "net"
    , graphBackgroundColor = graphBGColor
    , graphBorderColor = graphBGColor
    }

batBarCfg = (defaultBarConfig colorFunc)
    { barDirection = HORIZONTAL
    , barWidth = 48
    }
  where
    colorFunc pct
      | pct < 0.2 = (1.0, 0.0, 0.0)
      | pct < 0.3 = (0.8, 0.4, 0.0)
      | pct < 0.5 = (0.5, 0.5, 0.0)
      | pct < 0.7 = (0.3, 0.7, 0.0)
      | pct == 1  = (0.5, 0.5, 1.0)
      | otherwise = (0.0, 1.0, 0.0)


-- wifiMonitorNew = do
--     label <- pollingLabelNew "Wi-Fi" 0.5 pollWifiStr
--     widgetShowAll label
--     return $ toWidget label
--   where
--     format = formatNetworkInfo
--     pollWifiStr = do
--         essid <- maybePoll $ readProcess "iwgetid" ["-r"] ""
--         quality <- maybePoll $ getSignalQuality
--         return $ format (stripEnd '\n' essid ++ " " ++ stripEnd '.' quality)
--     -- | Parse signal quality out of /proc/net/wireless
--     getSignalQuality :: IO String
--     getSignalQuality = do
--         infoLines <- lines <$> readFile "/proc/net/wireless"
--         if length infoLines >= 3
--             then return (words (infoLines !! 2) !! 2)
--             else return "no network"
