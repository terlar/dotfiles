import XMonad
import XMonad.Util.EZConfig (checkKeymap, additionalKeysP)

import Config
import Hooks (myManageHook, myLogHook, myLayoutHook)
import Topics (myTopics)
import Keys (myKeys)

myConfig = defaultConfig
    { modMask = myModMask
    , terminal = myTerminal
    , borderWidth = myBorderWidth
    , normalBorderColor = myBorderColor
    , focusedBorderColor = myFocusedBorderColor
    , workspaces = myTopics
    , manageHook = myManageHook
    , logHook = myLogHook
    , layoutHook = myLayoutHook
    , startupHook = return () >> checkKeymap myConfig myKeys
    } `additionalKeysP` myKeys

main = xmonad myConfig
