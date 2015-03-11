module Config where

import XMonad
import XMonad.Prompt
import XMonad.Actions.GridSelect

myModMask = mod4Mask

myTerminal = "termite"
myBrowser = "firefox"
myPDFViewer = "zathura"
myImageViewer = "feh"
myVideoPlayer = "mplayer"

myBorderWidth :: Dimension
myBorderWidth = 0

-- Theme
myFont = "Input Mono-12"

myBorderColor = "#b0b0b0"
myFocusedFGColor = "#505050"
myFocusedBGColor = "#b0b0b0"

myFGColor = "#2a2e6e"
myBGColor = "#f5f5f5"

dmenuConfig :: String
dmenuConfig =
    [ ("-fn", myFont)
    , ("-nf", myFGColor)
    , ("-nb", myBGColor)
    , ("-sf", myFocusedFGColor)
    , ("-sb", myFocusedBGColor)
    ] >>= \ (opt, val) -> (" " ++ opt ++ " '" ++ val ++ "'")

myXPConfig :: XPConfig
myXPConfig = defaultXPConfig
    { font = "xft:" ++ myFont
    , fgColor = myFGColor
    , bgColor = myBGColor
    , fgHLight = myFocusedFGColor
    , bgHLight = myFocusedBGColor
    , borderColor = myBorderColor
    , promptBorderWidth = 1
    , height = 23
    , position = Top
    , historySize = 100
    , historyFilter = deleteConsecutive
    }

myGSConfig colorizer = (buildDefaultGSConfig colorizer)
    { gs_cellheight = 40
    , gs_cellwidth = 300
    , gs_cellpadding = 10
    , gs_font = "xft:" ++ myFont
    }

myColorizer = colorRangeFromClassName
    black -- lowest inactive bg
    white -- highest inactive bg
    white -- active bg
    white -- inactive fg
    black -- active fg
  where
    black = minBound
    white = (0xF5, 0xF5, 0xF5)
