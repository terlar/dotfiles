module Keys where

import XMonad
import XMonad.Prompt.Shell
import XMonad.Actions.FloatKeys
import XMonad.Actions.CycleWS
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.GridSelect
import XMonad.Actions.Navigation2D
import XMonad.Actions.WithAll
import XMonad.Actions.Search
import XMonad.Actions.WindowGo
import XMonad.Actions.Warp
import XMonad.Util.NamedScratchpad
import XMonad.Util.Paste
import XMonad.Layout.Reflect
import XMonad.Layout.MultiToggle
import XMonad.Layout.GridVariants(ChangeMasterGridGeom(IncMasterCols, IncMasterRows))

import qualified XMonad.Hooks.ManageDocks as M
import qualified XMonad.StackSet as W

-- Data module
import Data.Maybe(isJust)

import Config
import Utils
import Hooks
import Topics

myKeys :: [(String, X ())]
myKeys =
    -- Recompile and restart XMonad
    [ ("M-q", restartXMonad)
    -- Layout
    , ("M-\\", sendMessage $ Toggle REFLECTX)
    , ("M-S-\\", sendMessage $ Toggle REFLECTY)
    , ("M-S--", sendMessage $ IncMasterCols 1)
    , ("M-S-=", sendMessage $ IncMasterCols (-1))
    , ("M-C--", sendMessage $ IncMasterRows 1)
    , ("M-C-=", sendMessage $ IncMasterRows (-1))
    , ("M-b", sendMessage M.ToggleStruts)
    -- Grid select
    , ("M-g", selectWS)
    , ("M-S-g", takeToWS)
    , ("<XF86LaunchA>", selectWindow)
    , ("<XF86LaunchB>", spawnApp)
    , ("M-w", selectWindow)
    , ("M-S-w", bringWindow)
    -- Screen navigation
    , ("M-S-j", screenGo M.D False >> bringMouse)
    , ("M-S-k", screenGo M.U False >> bringMouse)
    -- Workspace navigation
    , ("M-<Tab>", toggleWS' ["NSP"])
    , ("M-]", moveTo Next nonSPAndNonEmptyWS)
    , ("M-[", moveTo Prev nonSPAndNonEmptyWS)
    , ("M-S-]", shiftToNext >> nextWS)
    , ("M-S-[", shiftToPrev >> prevWS)
    -- Dynamic workspaces
    , ("M-n", addWorkspacePrompt myXPConfig)
    , ("M-<Backspace>", killAll >> removeWorkspace >> createOrGoto "dashboard")
    , ("M-c", renameWorkspace myXPConfig)
    -- Scratchpads
    , ("M-`", scratchToggle "scratchpad")
    , ("M-<XF86AudioMute>", scratchToggle "pavucontrol")
    , ("M-m", scratchToggle "ncmpcpp")
    -- Global window
    , ("M-z", toggleGlobal)
    -- Shell prompt
    , ("M-p", programLauncher)
    , ("M-S-p", shellPrompt myXPConfig)
    -- Lock screen
    , ("M-<Esc>", spawn "i3lock -i ~/pictures/saltside.png -c 000000" )
    -- Screenshot
    , ("M-<F12>", spawn "scrot")
    -- Partial screenshot
    , ("M-S-<F12>", spawn "sleep 0.2; scrot -s")
    -- Notifications
    , ("M-8", spawn "notify-send -i network -t 4000 Network \"$(ip -4 -o addr show | cut -d' ' -f2,7)\"")
    , ("M-9", spawn "notify-send -i battery -t 2000 Battery \"$(acpi)\"")
    , ("M-0", spawn "notify-send -i dialog-information -t 2000 Date \"$(date)\"")
    -- Key sequences
    , ("M-v", sendKey shiftMask xK_Insert)
    ]
    ++ [("M-a " ++ k, createOrGoto t) | (k,t) <- workspaces]
    ++ [("M-d " ++ k, f) | (k,f) <- utils]
    ++ [("M-s " ++ k, promptSearch myXPConfig e) | (k,e) <- searches]
    ++ screenKeys ++ windowKeys ++ mediaKeys
  where
    nonSPAndNonEmptyWS = WSIs $ nonSPAndNonEmptyWS' ["NSP"]

    workspaces =
        [ ("1", "dashboard")
        , ("n", "note")
        , ("c", "code")
        , ("w", "web")
        , ("m", "music")
        , ("v", "video")
        , ("p", "pdf")
        , ("f", "file")
        , ("s", "speak")
        , ("d", "doc")
        ]

    utils =
        [ ("a", spawnApp)
        , ("e", spawnEditor)
        , ("w", spawn myBrowser)
        , ("f", spawnFile)
        ]

    searches =
        [ ("g", google)
        , ("w", wikipedia)
        , ("d", dictionary)
        , ("t", thesaurus)
        , ("y", youtube)
        ]

    -- Scratchpad invocation
    scratchToggle a = namedScratchpadAction myScratchpads a >> bringMouse

    -- GridSelect actions
    spawnApp     = runSelectedAction (myGSConfig pink) myApps
    selectWindow = goToSelected (myGSConfig blue) >> windows W.swapMaster >> bringMouse
    bringWindow  = bringSelected (myGSConfig orange) >> bringMouse
    selectWS     = gridselectWorkspace (myGSConfig green) W.greedyView >> bringMouse
    takeToWS     = gridselectWorkspace (myGSConfig purple) (\ws -> W.greedyView ws . W.shift ws) >> bringMouse

    -- Colors
    blue   = myColor "#25629f"
    green  = myColor "#629f25"
    orange = myColor "#9f6225"
    pink   = myColor "#9f2562"
    purple = myColor "#62259f"

screenKeys :: [(String, X ())]
screenKeys =
    [ mask ++ [key] ~> screenWorkspace s >>= flip whenJust (windows . action)
    | (key, s) <- zip "123" [0..]
    , (mask, action) <- [("M-", W.view), ("M-S-", W.shift)]
    ]

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
    [ ("<XF86MonBrightnessUp>", spawn "xbacklight -inc 40")   -- Monitor brighness up
    , ("<XF86MonBrightnessDown>", spawn "xbacklight -dec 40") -- Monitor brighness down
    , ("<XF86KbdBrightnessUp>", spawn "kbdlight up")          -- Keyboard brighness up
    , ("<XF86KbdBrightnessDown>", spawn "kbdlight down")      -- Keyboard brighness down

    , ("<XF86AudioPlay>", mpcAction "toggle")        -- Play/Pause track
    , ("<XF86AudioStop>", mpcAction "stop")          -- Stop track
    , ("<XF86AudioNext>", mpcAction "next")          -- Next track
    , ("<XF86AudioPrev>", mpcAction "prev")          -- Previous track
    , ("<XF86AudioLowerVolume>", amixerAction "5%-") -- Decrease volume
    , ("<XF86AudioRaiseVolume>", amixerAction "5%+") -- Increase volume
    , ("<XF86AudioMute>", amixerAction "toggle")     -- Mute volume
    ]
  where
    mpcAction opt = spawn $ unwords ["mpc", opt]
    amixerAction opt = spawn $ unwords ["amixer", "-q", "set", "Master", opt]

-- Menus
myApps =
    [ ("Firefox",      raiseApp  "web" "firefox")
    , ("GVim",         raiseApp' "gvim")
    , ("LibreOffice",  raiseApp  "doc" "libreoffice")
    , ("Themes",       spawn     "lxappearance")
    ]
  where
    raiseApp ws a = raiseNextMaybe (spawnWS ws a) (appName ~? a) >> bringMouse
    raiseApp' a = raiseNextMaybe (spawn a) (appName ~? a) >> bringMouse
    myRaiseTerm a d = raiseNextMaybe (spawnWS a (termApp a d)) (role ~? a) >> bringMouse
    termApp a d = myTerm ++ " -r " ++ a ++ " -d " ++ d
    -- Named Workspace Navigation
    spawnWS ws a = addWorkspace ws >> spawn a

nonSPAndNonEmptyWS' s = return (\w -> (W.tag w `notElem` s) && isJust (W.stack w))

-- Warp mouse
bringMouse = warpToWindow (9/10) (9/10)
