module Config where

import XMonad
import XMonad.Prompt
import XMonad.Actions.GridSelect

myModMask = mod4Mask

myTerm = "termite"
myBrowser = "firefox"
myPDFViewer = "zathura"
myImageViewer = "feh"
myVideoPlayer = "mplayer"

myBorderWidth :: Dimension
myBorderWidth = 4

-- Theme
myFont = "Input Mono-8"

myBGColor     = "#f5f5f5" -- Background color
myFGColor     = "#2a2e6e" -- Foreground color
myHLight      = "#ecf0f1" -- Highlight color
myLLight      = "#2c3e50" -- Lowlight color
myBorderColor = "#bdc3c7" -- Border color

dmenuConfig :: String
dmenuConfig =
    [ ("-fn", myFont)
    , ("-nf", myFGColor)
    , ("-nb", myBGColor)
    , ("-sf", myHLight)
    , ("-sb", myLLight)
    ] >>= \ (opt, val) -> (" " ++ opt ++ " '" ++ val ++ "'")

myXPConfig :: XPConfig
-- XPConfig - Prompt fields
myXPConfig = defaultXPConfig
    { font = "xft:" ++ myFont
    , fgColor = myFGColor
    , bgColor = myBGColor
    , fgHLight = myHLight
    , bgHLight = myLLight
    , borderColor = myBGColor
    , promptBorderWidth = 1
    , height = 30
    , position = Top
    , historySize = 100
    , historyFilter = deleteConsecutive
    }

myGSConfig colorizer = (buildDefaultGSConfig colorizer)
    { gs_cellheight = 50
    , gs_cellwidth = 300
    , gs_cellpadding = 10
    , gs_font = "xft:" ++ myFont
    }

myColor color _ isFg = return $
    if isFg
    then (color, myHLight)
    else (myHLight, color)
