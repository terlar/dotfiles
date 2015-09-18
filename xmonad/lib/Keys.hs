module Keys where

import XMonad
import XMonad.Prompt.Shell
import XMonad.Actions.FloatKeys
import XMonad.Actions.CycleWS
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.GridSelect
import XMonad.Actions.WithAll
import XMonad.Actions.Search
import XMonad.Util.NamedScratchpad
import XMonad.Util.Paste

import qualified XMonad.StackSet as W
import Data.Maybe (isJust)

import Config
import Utils
import Hooks
import Topics

myKeys :: [(String, X ())]
myKeys =
    -- Recompile and restart XMonad
    [ ("M-q", restartXMonad)
    -- Grid select
    , ("M-g", openGridSelect)
    , ("<XF86LaunchA>", openGridSelect)
    -- Workspace navigation
    , ("M-<Tab>", myToggleWS)
    , ("M-]", moveTo Next nonSPAndNonEmptyWS)
    , ("M-[", moveTo Prev nonSPAndNonEmptyWS)
    , ("M-S-]", shiftToNext >> nextWS)
    , ("M-S-[", shiftToPrev >> prevWS)
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
    -- Scratchpads
    , ("M-`", namedScratchpadAction myScratchpads "scratchpad")
    , ("M-<XF86AudioMute>", namedScratchpadAction myScratchpads "volume")
    , ("M-m", namedScratchpadAction myScratchpads "music")
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
    -- Notifications
    , ("M-8", spawn "notify-send -i network -t 4000 Network \"$(ip -4 -o addr show | cut -d' ' -f2,7)\"")
    , ("M-9", spawn "notify-send -i battery -t 2000 Battery \"$(acpi)\"")
    , ("M-0", spawn "notify-send -i dialog-information -t 2000 Date \"$(date)\"")
    -- Key sequences
    , ("M-v", sendKey shiftMask xK_Insert)
    ]
    ++ windowKeys ++ mediaKeys
  where
    openGridSelect = goToSelected $ myGSConfig myColorizer
    nonSPAndNonEmptyWS = WSIs $ nonSPAndNonEmptyWS' ["NSP"]

windowKeys :: [(String, X ())]
windowKeys =
    -- Moving floating window with key
    [ (c ++ m ++ k, withFocused $ f (d x))
    | (d, k) <- zip [\a->(a, 0), \a->(0, a), \a->(-a, 0), \a->(0, -a)] ["<R>", "<D>", "<L>", "<U>"]
    , (f, m) <- zip [keysMoveWindow, \d -> keysResizeWindow d (0, 0)] ["M-", "M-S-"]
    , (c, x) <- zip ["", "C-"] [20, 2]
    ]


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
    mpcAction opt = spawn $ unwords ["mpc", opt]
    amixerAction opt = spawn $ unwords ["amixer", "-q", "set", "Master", opt]

nonSPAndNonEmptyWS' s = return (\w -> (W.tag w `notElem` s) && isJust (W.stack w))
myToggleWS = windows $ W.view =<< W.tag . head . filter ((\x -> x /= "NSP" && x /= "SP") . W.tag) . W.hidden
