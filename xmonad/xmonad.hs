import XMonad
import XMonad.Util.EZConfig (checkKeymap, additionalKeysP)

import Config
import Hooks (myManageHook, myLogHook)
import Topics (myTopics)
import Keys (myKeys)

myConfig = defaultConfig
    { modMask = myModMask
    , terminal = myTerminal
    , borderWidth = myBorderWidth
    , workspaces = myTopics
    , manageHook = myManageHook
    , logHook = myLogHook
    , startupHook = return () >> checkKeymap myConfig myKeys
    } `additionalKeysP` myKeys

main = xmonad myConfig
