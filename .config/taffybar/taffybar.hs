import Control.Applicative
import Control.Exception

import System.Taffybar

import System.Taffybar.Systray
import System.Taffybar.TaffyPager
import System.Taffybar.Pager
import System.Taffybar.SimpleClock
import System.Taffybar.FreedesktopNotifications
import System.Taffybar.Weather
import System.Taffybar.MPRIS
import System.Taffybar.CPUMonitor
import System.Taffybar.DiskIOMonitor
import System.Taffybar.NetMonitor
import System.Taffybar.Battery

import System.Taffybar.Widgets.PollingBar
import System.Taffybar.Widgets.PollingLabel
import System.Taffybar.Widgets.PollingGraph

import System.Information.Memory
import System.Information.CPU

import Data.List
import Graphics.UI.Gtk
import System.Process


formatNetworkInfo :: String -> String
formatNetworkInfo str = "<span color='yellow'>" ++ str ++ "</span>"

memCallback = do
  mi <- parseMeminfo
  return [memoryUsedRatio mi]

wifiMonitorNew = do
    label <- pollingLabelNew "Wi-Fi" 0.5 pollWifiStr
    widgetShowAll label
    return $ toWidget label
  where
    format = formatNetworkInfo
    pollWifiStr = do
        essid <- maybePoll $ readProcess "iwgetid" ["-r"] ""
        quality <- maybePoll $ getSignalQuality
        return $ format (stripEnd '\n' essid ++ " " ++ stripEnd '.' quality)
    stripEnd ch [] = []
    stripEnd ch str
        | [ch] `isSuffixOf` str = stripEnd ch $ init str
        | otherwise = str
    catchAny :: IO a -> (SomeException -> IO a) -> IO a
    catchAny = Control.Exception.catch
    maybePoll func = catchAny func (\_ -> return "-")
    -- | Parse signal quality out of /proc/net/wireless
    getSignalQuality :: IO String
    getSignalQuality = do
        infoLines <- lines <$> readFile "/proc/net/wireless"
        if length infoLines >= 3
           then return (words (infoLines !! 2) !! 2)
           else return "no network"

main = do
  let clock = textClockNew Nothing "<span fgcolor='orange'>%a %b %_d %H:%M</span>" 1
      pager = taffyPagerNew pagerCfg
      note = notifyAreaNew defaultNotificationConfig
      mpris = mprisNew defaultMPRISConfig
      cpu = cpuMonitorNew cpuGraphCfg 0.5 "cpu"
      mem = pollingGraphNew memGraphCfg 1 memCallback
      ssdIO = dioMonitorNew ssdIOGraphCfg 0.5 "sdb"
      hddIO = dioMonitorNew hddIOGraphCfg 0.5 "sda"
      wifi = wifiMonitorNew
      net = netMonitorNewWith 0.5 "wlp3s0" 2 (formatNetworkInfo defaultNetFormat)
      batBar = batteryBarNew batBarCfg 1
      tray = systrayNew
  defaultTaffybar defaultTaffybarConfig
       { startWidgets = [ pager, note ]
       , endWidgets = reverse [ net, wifi, cpu, mem, ssdIO, hddIO, mpris, batBar, clock, tray ]
       , barPosition = Bottom
       , widgetSpacing = 16
       }


cpuGraphCfg = defaultGraphConfig
    { graphDataColors = [ (0, 1, 0, 1), (1, 0, 1, 0.5) ]
    , graphLabel = Just "cpu"
    }

memGraphCfg = defaultGraphConfig
    { graphDataColors = [(1, 0, 0, 1)]
    , graphLabel = Just "mem"
    }

ssdIOGraphCfg = defaultGraphConfig
    { graphDataColors = [ (0, 1, 0, 0.8), (1, 0, 0, 0.8) ]
    , graphLabel = Just "ssd"
    }

hddIOGraphCfg = defaultGraphConfig
    { graphDataColors = [ (0, 1, 0, 0.8), (1, 0, 0, 0.8) ]
    , graphLabel = Just "hdd"
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

pagerCfg = defaultPagerConfig
    { emptyWorkspace = colorize "gray" "" . escape
    }
