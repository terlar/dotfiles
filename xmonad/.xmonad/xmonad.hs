import XMonad
import XMonad.Util.EZConfig (checkKeymap, additionalKeysP)
import XMonad.Hooks.UrgencyHook

import Config
import Hooks (myHooks, myLogHook, myLayoutHook)
import Topics (myTopics)
import Keys (myKeys)

myConfig = withUrgencyHook NoUrgencyHook $ defaultConfig
    { modMask = myModMask
    , terminal = myTerm
    , borderWidth = myBorderWidth
    , normalBorderColor = myBorderColor
    , focusedBorderColor = myLLight
    , workspaces = myTopics
    , manageHook = myHooks
    , logHook = myLogHook
    , layoutHook = myLayoutHook
    , startupHook = return () >> checkKeymap myConfig myKeys
    } `additionalKeysP` myKeys

main = xmonad myConfig
