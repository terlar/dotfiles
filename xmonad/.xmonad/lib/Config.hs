module Config where

import           XMonad
import           XMonad.Prompt

import           XMonad.Actions.GridSelect
import           XMonad.Actions.ShowText

import           XMonad.Layout.Tabbed

import           Data.Default              (def)

myModMask :: KeyMask
myModMask = mod4Mask

myTerm, myEditor, myBrowser, myPDFViewer, myImageViewer, myVideoPlayer :: String

myTerm = "alacritty"
myEditor = "emacsclient -c"
myBrowser = "qutebrowser"
myPDFViewer = "zathura"
myImageViewer = "feh"
myVideoPlayer = "mpv"

myBorderWidth :: Dimension
myBorderWidth = 4

-- Theme
myFont :: String
myFont = "xft:sans-10"

myBGColor, myFGColor, myHLight, myLLight, myBorderColor :: String
myBGColor        = "#f5f5f5" -- Background color
myFGColor        = "#2a2e6e" -- Foreground color
myHLight         = "#ecf0f1" -- Highlight color
myLLight         = "#2c3e50" -- Lowlight color
myBorderColor    = "#bdc3c7" -- Border color
myAttentionColor = "#ef5350" -- Attention color

-- XPConfig - Prompt fields
myXPConfig :: XPConfig
myXPConfig = def
    { font = myFont
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

myTabConfig :: Theme
myTabConfig = def
    { fontName = myFont
    , decoHeight = 30
    , activeBorderColor   = myBGColor
    , activeTextColor     = myHLight
    , activeColor         = myLLight
    , inactiveBorderColor = myBGColor
    , inactiveTextColor   = myFGColor
    , inactiveColor       = myBGColor
    , urgentBorderColor   = myAttentionColor
    , urgentTextColor     = myHLight
    , urgentColor         = myAttentionColor
    }

myGSConfig :: (a -> Bool -> X (String, String)) -> GSConfig a
myGSConfig colorizer = (buildDefaultGSConfig colorizer)
    { gs_cellheight = 50
    , gs_cellwidth = 300
    , gs_cellpadding = 10
    , gs_font = myFont
    }

myTextConfig :: ShowTextConfig
myTextConfig = STC
    { st_font = "xft:sans-72"
    , st_bg   = "#020202"
    , st_fg   = "#a9a6af"
    }

myColor color _ isFg = return $
    if isFg
    then (color, myHLight)
    else (myHLight, color)
