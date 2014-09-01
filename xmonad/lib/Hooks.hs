module Hooks where

import XMonad
import qualified XMonad.StackSet as W
import XMonad.ManageHook
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.FadeInactive
import XMonad.Actions.CopyWindow
import XMonad.Util.NamedScratchpad

import Utils
import Topics

myManageHook :: ManageHook
myManageHook = composeAll . concat $
    [ [ className =? x --> doCenterFloat | x <- floats ]
    , [ role =? "scratchpad" --> doCenterFloat ]
    , [ isDialog --> doCenterFloat ]
    , [ isFullscreen --> doFullFloat ]
    ]
  where
    floats =
        [ "feh"
        , "Zenity"
        , "Gcolor2"
        , "Pavucontrol"
        , "Arandr"
        , "Lxappearance"
        , "Qtconfig-qt4"
        ]

scratchpads :: NamedScratchpads
scratchpads =
    [ NS "scratchpad" scratchpad (role =? "scratchpad") nonFloating
    , NS "volume" "pavucontrol" (className =? "Pavucontrol") nonFloating
    ]
  where
    scratchpad = "termite -r scratchpad"

myLogHook :: X ()
myLogHook = fadeInactiveLogHook fadeAmount
  where fadeAmount = 0.9

myToggleWS = windows $
    W.view =<< W.tag . head . filter ((/= "NSP") . W.tag) . W.hidden
