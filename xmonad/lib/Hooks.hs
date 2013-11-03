module Hooks
     ( myManageHook
     , myToggleWS
     ) where

import XMonad
import XMonad.ManageHook
import XMonad.Hooks.ManageHelpers

import XMonad.Actions.CopyWindow

import qualified XMonad.StackSet as W

import Topics

myManageHook :: ManageHook
myManageHook =
    (composeAll . concat $
    [ [ isFullscreen          --> doFullFloat ]
    , [ isDialog              --> doCenterFloat ]
    , [ className =? "Zenity" --> ask >>= doF . W.sink ]
    , [ className =? x        --> doCenterFloat | x <- floats ]
    ])
    where
      floats = ["feh", "Pavucontrol"]

      doMaster = doF W.shiftMaster
      doCenterFloat' = doCenterFloat <+> doMaster
      doFloatAt' x y = doFloatAt x y <+> doMaster
      doSideFloat' p = doSideFloat p <+> doMaster
      doRectFloat' r = doRectFloat r <+> doMaster
      doFullFloat' = doFullFloat <+> doMaster
      doShiftAndGo ws = doF (W.greedyView ws) <+> doShift ws
      doCopyToAll = ask >>= doF . \w -> (\ws -> foldr($) ws (map (copyWindow w) myTopics))
      doCenterFloatToAll = doCopyToAll <+> doCenterFloat'

myToggleWS = windows $ W.view =<< W.tag . head . filter ((\x -> x /= "NSP") . W.tag) . W.hidden
