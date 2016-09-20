import           XMonad
import           XMonad.Hooks.UrgencyHook
import           XMonad.Util.EZConfig     (additionalKeysP, checkKeymap)

import           Config
import           Hooks                    (myEventHook, myLayoutHook, myLogHook,
                                           myManageHook)
import           Keys                     (myKeys)
import           Topics                   (myTopics)

myConfig = withUrgencyHook NoUrgencyHook $ defaultConfig
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
    } `additionalKeysP` myKeys

main = xmonad myConfig
