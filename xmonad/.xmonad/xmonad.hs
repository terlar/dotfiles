import           Data.Default              (def)
import           XMonad
import           XMonad.Util.EZConfig      (additionalKeysP,
                                            additionalMouseBindings,
                                            checkKeymap)

import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.UrgencyHook

import           Config
import           Hooks                     (myEventHook, myLayoutHook,
                                            myLogHook, myManageHook,
                                            myUrgencyHook)
import           Keys                      (myKeys)
import           Topics                    (myTopics)

myConfig = def
    { modMask = myModMask
    , terminal = myTerm
    , borderWidth = myBorderWidth
    , normalBorderColor = myBorderColor
    , focusedBorderColor = myLLight
    , focusFollowsMouse = False
    , workspaces = myTopics
    , layoutHook = myLayoutHook
    , startupHook = return () >> checkKeymap myConfig myKeys
    , manageHook = myManageHook
    , handleEventHook = myEventHook
    , logHook = myLogHook
    }
    `additionalKeysP`         myKeys

main = xmonad
     $ withUrgencyHook myUrgencyHook
     $ ewmh
     $ myConfig
