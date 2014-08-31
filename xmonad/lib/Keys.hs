module Keys where

import XMonad
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.GridSelect
import XMonad.Actions.WithAll

import Config
import Utils
import Hooks
import Topics

myKeys :: [(String, X ())]
myKeys =
    -- Recompile and restart XMonad
    [ ("M-q", restartXMonad)
    -- Grid select
    , ("M-g", goToSelected defaultGSConfig)
    , ("<XF86LaunchA>", goToSelected defaultGSConfig)
    -- Workspace navigation
    , ("M-z", promptedGoto)
    , ("M-S-z", promptedShift)
    -- Dynamic workspaces
    , ("M-n", addWorkspacePrompt myXPConfig)
    , ("M-<Backspace>", killAll >> removeWorkspace >> createOrGoto "dashboard")
    , ("M-c", renameWorkspace myXPConfig)
    -- Toggle workspace
    , ("M-<Tab>", myToggleWS)
    -- Global window
    , ("M-S-g", toggleGlobal)
    -- Workspace Keys
    , ("M-s 1", createOrGoto "dashboard")
    , ("M-s n", createOrGoto "note")
    , ("M-s c", createOrGoto "code")
    , ("M-s w", createOrGoto "web")
    , ("M-s m", createOrGoto "music")
    , ("M-s v", createOrGoto "video")
    , ("M-s p", createOrGoto "pdf")
    , ("M-s f", createOrGoto "file")
    , ("M-s s", createOrGoto "speak")
    -- Launcher
    , ("M-p", programLauncher)
    -- Launch editor
    , ("M-x e", spawnEditor)
    -- Launch browser
    , ("M-x w", spawn myBrowser)
    -- Browse files
    , ("M-x f", spawnFile)
    -- Volume control
    , ("M-v", spawn "pavucontrol")
    -- Lock screen
    , ("M-<Esc>", spawn "i3lock -i ~/pictures/saltside.png -c 000000" )
    -- Screenshot
    , ("M-`", spawn "scrot")
    -- Partial screenshot
    , ("M-S-`", spawn "sleep 0.2; scrot -s")
    ]
    ++ mediaKeys

mediaKeys :: [(String, X ())]
mediaKeys =
    -- Monitor brighness up
    [ ("<XF86MonBrightnessUp>", spawn "xbacklight -inc 40")
    -- Monitor brighness down
    , ("<XF86MonBrightnessDown>", spawn "xbacklight -dec 40")
    -- Keyboard brighness up
    , ("<XF86KbdBrightnessUp>", spawn "kbdlight up")
    -- Keyboard brighness down
    , ("<XF86KbdBrightnessDown>", spawn "kbdlight down")
    -- Play/Pause track
    , ("<XF86AudioPlay>", mpcAction "toggle")
    -- Stop track
    , ("<XF86AudioStop>", mpcAction "stop")
    -- Next track
    , ("<XF86AudioNext>", mpcAction "next")
    -- Previous track
    , ("<XF86AudioPrev>", mpcAction "prev")
    -- Decrease volume
    , ("<XF86AudioLowerVolume>", amixerAction "5%-")
    -- Increase volume
    , ("<XF86AudioRaiseVolume>", amixerAction "5%+")
    -- Mute volume
    , ("<XF86AudioMute>", amixerAction "toggle")
    ]
  where
    mpcAction opt = spawn $ unwords ["ncmpcpp", opt]
    amixerAction opt = spawn $ unwords ["amixer", "-q", "set", "Master", opt]
