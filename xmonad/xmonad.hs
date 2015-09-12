import XMonad
import XMonad.Util.EZConfig (checkKeymap, additionalKeysP)
import XMonad.Layout.PerWorkspace (onWorkspace)

import Config
import Hooks (myManageHook, myLogHook)
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
    , layoutHook = onWorkspace "speak" Full $ layoutHook defaultConfig
    , startupHook = return () >> checkKeymap myConfig myKeys
    } `additionalKeysP` myKeys

main = xmonad myConfig
