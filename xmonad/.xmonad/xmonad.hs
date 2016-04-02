import XMonad
import XMonad.Util.EZConfig (checkKeymap, additionalKeysP)
import XMonad.Hooks.SetWMName (setWMName)

import Config
import Hooks (myHooks, myLogHook, myLayoutHook)
import Topics (myTopics)
import Keys (myKeys)

myConfig = defaultConfig
    { modMask = myModMask
    , terminal = myTerm
    , borderWidth = myBorderWidth
    , normalBorderColor = myBorderColor
    , focusedBorderColor = myLLight
    , workspaces = myTopics
    , manageHook = myHooks
    , logHook = myLogHook
    , layoutHook = myLayoutHook
    , startupHook = setWMName "LG3D" >> checkKeymap myConfig myKeys
    } `additionalKeysP` myKeys

main = xmonad myConfig
