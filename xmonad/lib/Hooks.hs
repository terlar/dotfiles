module Hooks
     ( myManageHook
     , myToggleWS
     ) where

import XMonad
import XMonad.ManageHook
import XMonad.Hooks.ManageHelpers

import qualified XMonad.StackSet as W

myManageHook :: ManageHook
myManageHook =
    (composeAll . concat $
    [ [ isFullscreen                --> doFullFloat ]
    , [ isDialog                    --> doCenterFloat ]
    , [ className =? x              --> doCenterFloat | x <- floats ]
    ])
    where
      floats = ["feh", "Pavucontrol"]

      doMaster = doF W.shiftMaster
      doCenterFloat' = doCenterFloat <+> doMaster
      doFloatAt' x y = doFloatAt x y <+> doMaster
      doSideFloat' p = doSideFloat p <+> doMaster
      doRectFloat' r = doRectFloat r <+> doMaster
      doFullFloat' = doFullFloat <+> doMaster

myToggleWS = windows $ W.view =<< W.tag . head . filter ((\x -> x /= "NSP") . W.tag) . W.hidden
