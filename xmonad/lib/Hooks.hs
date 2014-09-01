module Hooks where

import XMonad
import XMonad.ManageHook
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.FadeInactive
import XMonad.Actions.CopyWindow
import qualified XMonad.StackSet as W

import Topics

myManageHook :: ManageHook
myManageHook = composeAll . concat $
    [ [ isFullscreen --> doFullFloat ]
    , [ isDialog --> doCenterFloat ]
    , [ className =? "Zenity" --> ask >>= doF . W.sink ]
    , [ className =? x --> doCenterFloat | x <- floats ]
    ]
  where
    floats =
        [ "feh"
        , "Gcolor2"
        , "Pavucontrol"
        , "Arandr"
        , "Lxappearance"
        , "Qtconfig-qt4"
        ]

    doMaster = doF W.shiftMaster
    doCenterFloat' = doCenterFloat <+> doMaster
    doFloatAt' x y = doFloatAt x y <+> doMaster
    doSideFloat' p = doSideFloat p <+> doMaster
    doRectFloat' r = doRectFloat r <+> doMaster
    doFullFloat' = doFullFloat <+> doMaster
    doShiftAndGo ws = doF (W.greedyView ws) <+> doShift ws
    doCopyToAll = ask >>= doF . \ w ws -> (foldr (copyWindow w) ws myTopics)
    doCenterFloatToAll = doCopyToAll <+> doCenterFloat'

myLogHook :: X ()
myLogHook = fadeInactiveLogHook fadeAmount
  where fadeAmount = 0.9

myToggleWS = windows $
    W.view =<< W.tag . head . filter ((/= "NSP") . W.tag) . W.hidden
