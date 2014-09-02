module Hooks where

import XMonad
import qualified XMonad.StackSet as W
import XMonad.ManageHook
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.FadeInactive
import XMonad.Actions.CopyWindow
import XMonad.Util.NamedScratchpad

import Config
import Utils

myManageHook :: ManageHook
myManageHook = (composeAll . concat $
    [ [ className =? x --> doCenterFloat | x <- floats ]
    , [ isDialog --> doCenterFloat ]
    , [ isFullscreen --> doFullFloat ]
    ]) <+> namedScratchpadManageHook myScratchpads
  where
    floats =
        [ "feh"
        , "Zenity"
        , "Gcolor2"
        , "Arandr"
        , "Lxappearance"
        , "Qtconfig-qt4"
        ]

myScratchpads :: NamedScratchpads
myScratchpads =
    [ NS "scratchpad" spawnSP findSP manageSP
    , NS "volume" "pavucontrol" (className =? "Pavucontrol") doCenterFloat
    ]
  where
    spawnSP = myTerminal ++ " -r scratchpad"
    findSP = role =? "scratchpad"
    manageSP = customFloating $ W.RationalRect l t w h
      where
        h = 0.75
        w = 0.75
        t = (1 - h) / 2
        l = (1 - w) / 2

myLogHook :: X ()
myLogHook = fadeInactiveLogHook fadeAmount
  where fadeAmount = 0.9

myToggleWS = windows $
    W.view =<< W.tag . head . filter ((/= "NSP") . W.tag) . W.hidden
