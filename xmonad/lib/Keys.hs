module Keys where

import XMonad
import XMonad.Prompt.Shell
import XMonad.Actions.CycleWS
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.GridSelect
import XMonad.Actions.WithAll
import XMonad.Actions.Search
import XMonad.Util.Scratchpad

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
    , ("M-<Tab>", myToggleWS)
    , ("M-<Right>", nextWS)
    , ("M-<Left>", prevWS)
    , ("M-S-<Right>", shiftToNext)
    , ("M-S-<Left>", shiftToPrev)
    , ("M-z", promptedGoto)
    , ("M-S-z", promptedShift)
    , ("M-a 1", createOrGoto "dashboard")
    , ("M-a n", createOrGoto "note")
    , ("M-a c", createOrGoto "code")
    , ("M-a w", createOrGoto "web")
    , ("M-a m", createOrGoto "music")
    , ("M-a v", createOrGoto "video")
    , ("M-a p", createOrGoto "pdf")
    , ("M-a f", createOrGoto "file")
    , ("M-a s", createOrGoto "speak")
    -- Dynamic workspaces
    , ("M-n", addWorkspacePrompt myXPConfig)
    , ("M-<Backspace>", killAll >> removeWorkspace >> createOrGoto "dashboard")
    , ("M-c", renameWorkspace myXPConfig)
    , ("M-`", scratchpadSpawnAction defaultConfig { terminal = myTerminal })
    -- Global window
    , ("M-S-g", toggleGlobal)
    -- Launcher
    , ("M-p", programLauncher)
    -- Run
    , ("M-S-x", shellPrompt myXPConfig)
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
    , ("M-<F12>", spawn "scrot")
    -- Partial screenshot
    , ("M-S-<F12>", spawn "sleep 0.2; scrot -s")
    -- Web searches
    , ("M-s g", promptSearch myXPConfig google)
    , ("M-s w", promptSearch myXPConfig wikipedia)
    , ("M-s d", promptSearch myXPConfig dictionary)
    , ("M-s t", promptSearch myXPConfig thesaurus)
    , ("M-s y", promptSearch myXPConfig youtube)
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
