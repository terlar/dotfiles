module Config
     ( myFont
     , myXPConfig
     , myModMask
     , myTerminal
     , myFocusFollowsMouse
     , myClickJustFocuses
     , myBorderWidth
     , myWindowSpacing
     , myNormalBorderColor
     , myFocusedBorderColor
     ) where

import XMonad
import XMonad.Prompt

myModMask = mod4Mask
myTerminal = "termite"

myFocusFollowsMouse = True
myClickJustFocuses = False

myBorderWidth :: Dimension
myBorderWidth = 1

myWindowSpacing :: Int
myWindowSpacing = 2

myFont = "xft:Source Code Pro:pixelsize=12"

-- Colors
myNormalBorderColor  = defaultBG
myFocusedBorderColor = defaultFG

defaultFG = "#2a2e6e"
defaultBG = "#f5f5f5"

colorBlack  = "#151515"
colorRed    = "#c35532"
colorGreen  = "#7da028"
colorYellow = "#c19a48"
colorBlue   = "#008ebd"
colorPurple = "#9474b6"
colorCyan   = "#00adbe"
colorWhite  = "#b0b0b0"
colorGrey   = "#505050"

myXPConfig =
    defaultXPConfig
    { font              = myFont
    , fgColor           = defaultFG
    , bgColor           = defaultBG
    , fgHLight          = colorCyan
    , bgHLight          = colorBlack
    , borderColor       = colorBlack
    , promptBorderWidth = 1
    , height            = 20
    , position          = Top
    , historySize       = 100
    , historyFilter     = deleteConsecutive
    }
