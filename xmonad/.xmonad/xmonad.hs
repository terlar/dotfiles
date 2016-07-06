import XMonad
import XMonad.Util.EZConfig (checkKeymap, additionalKeysP)
import XMonad.Hooks.UrgencyHook

import Config
import Hooks (myManageHook, myLogHook, myLayoutHook)
import Topics (myTopics)
import Keys (myKeys)

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
    , logHook = myLogHook
    } `additionalKeysP` myKeys

main = xmonad myConfig
