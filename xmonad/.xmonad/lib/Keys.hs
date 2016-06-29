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
    -- Layout
    [ ("M-\\", sendMessage $ Toggle REFLECTX)
    , ("M-S-\\", sendMessage $ Toggle REFLECTY)
    , ("M-S--", sendMessage $ IncMasterCols 1)
    , ("M-S-=", sendMessage $ IncMasterCols (-1))
    , ("M-C--", sendMessage $ IncMasterRows 1)
    , ("M-C-=", sendMessage $ IncMasterRows (-1))
    , ("M-b", sendMessage M.ToggleStruts)
    -- Screen navigation
    , ("M-S-j", screenGo M.D False >> bringMouse)
    , ("M-S-k", screenGo M.U False >> bringMouse)
    -- Dynamic workspaces
    , ("M-- n", addWorkspacePrompt myXPConfig)
    , ("M-- r", renameWorkspace myXPConfig)
    , ("M-- k", killAll >> removeWorkspace >> createOrGoto "dashboard")
    -- Workspace navigation
    , ("M-<Tab>", toggleWS' ["NSP"])
    , ("M-]", moveTo Next nonEmptyWSNoNSP)
    , ("M-[", moveTo Prev nonEmptyWSNoNSP)
    , ("M-S-]", shiftTo Next nonEmptyWSNoNSP >> moveTo Next nonEmptyWSNoNSP)
    , ("M-S-[", shiftTo Prev nonEmptyWSNoNSP >> moveTo Prev nonEmptyWSNoNSP)
    -- Sticky global window
    , ("M-z", toggleGlobal)
    -- Grid select
    , ("<XF86LaunchB>", spawnApp)
    , ("<XF86LaunchA>", selectWindow)
    , ("S-<XF86LaunchA>", bringWindow)
    , ("M-w", selectWindow)
    , ("M-S-w", bringWindow)
    , ("M-s", selectWS)
    , ("M-S-s", takeToWS)
    -- Scratchpads
    , ("M-`"                     , scratchToggle "scratchpad")
    , ("M-e"                     , scratchToggle "editor")
    , ("M-<XF86AudioLowerVolume>", scratchToggle "volume")
    , ("M-<XF86AudioRaiseVolume>", scratchToggle "volume")
    , ("M-a m"                   , scratchToggle "music")
    , ("M-'"                     , scratchToggle "dictionary")
    -- Shell prompt
    , ("M-p", programLauncher)
    , ("M-S-p", shellPrompt myXPConfig)
    -- Password prompt
    , ("M-S-8", passPrompt)
    -- Lock screen
    , ("M-<Esc>", spawn "lock" )
    -- Display management
    , ("M-<F1>", spawn "autorandr --load mobile")
    , ("M-<F2>", spawn "autorandr --change --default mobile")
    -- Screenshot
    , ("M-<F11>", spawn "scrot -e 'mv $f ~/pictures/screenshots/'")
    , ("M-S-<F11>", spawn "scrot --multidisp -e 'mv $f ~/pictures/screenshots/'")
    , ("M-<F12>", spawn "scrot --focused -e 'mv $f ~/pictures/screenshots/'")
    , ("M-S-<F12>", spawn "sleep 0.3; scrot --select -e 'mv $f ~/pictures/screenshots/'")
    -- Notifications
    , ("M-n n", spawn "notify-send -i network -t 4000 Network \"$(ip -4 -o addr show | cut -d' ' -f2,7)\"")
    , ("M-n b", spawn "notify-send -i battery -t 2000 Battery \"$(acpi)\"")
    , ("M-n d", spawn "notify-send -i dialog-information -t 2000 Date \"$(date '+%F%nW%V %A%n%T')\"")
    ]
    ++ [("M-g " ++ k, createOrGoto t) | (k,t) <- workspaces]
    ++ [("M-r " ++ k, f) | (k,f) <- utils]
    ++ [("M-q " ++ k, promptSearch myXPConfig e) | (k,e) <- searches]
    ++ screenKeys ++ windowKeys ++ mediaKeys
  where
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

    -- Work space selection
    nonEmptyWSNoNSP = WSIs $ nonEmptyWSExcept ["NSP"]
    nonEmptyWSExcept s = return (\w -> (W.tag w `notElem` s) && isJust (W.stack w))

    -- Scratchpad invocation
    scratchToggle = namedScratchpadAction myScratchpads

    -- GridSelect actions
    spawnApp     = runSelectedAction (myGSConfig pink) myApps
    selectWindow = goToSelected (myGSConfig blue) >> windows W.swapMaster
    bringWindow  = bringSelected (myGSConfig orange)
    selectWS     = gridselectWorkspace (myGSConfig green) W.greedyView
    takeToWS     = gridselectWorkspace (myGSConfig purple) (\ws -> W.greedyView ws . W.shift ws)

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
    [ ("<XF86KbdBrightnessUp>", spawn "kbdlight up")
    , ("<XF86KbdBrightnessDown>", spawn "kbdlight down")
    ]

-- Menus
myApps :: [([Char], X ())]
myApps =
    [ ("Browser",      raiseApp  "web" myBrowser)
    , ("Editor",       raiseApp' myEditor)
    , ("LibreOffice",  raiseApp  "doc" "libreoffice")
    , ("Themes",       spawn     "lxappearance")
    ]
  where
    raiseApp ws a = raiseNextMaybe (spawnWS ws a) (appName ~? a)
    raiseApp' a = raiseNextMaybe (spawn a) (appName ~? a)
    myRaiseTerm a d = raiseNextMaybe (spawnWS a (termApp a d)) (roleName ~? a)
    termApp a d = myTerm ++ " -r " ++ a ++ " -d " ++ d
    -- Named Workspace Navigation
    spawnWS ws a = addWorkspace ws >> spawn a

-- Warp mouse
bringMouse :: X ()
bringMouse = warpToWindow (9/10) (9/10)
