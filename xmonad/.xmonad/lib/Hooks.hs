module Hooks where

import           XMonad                          hiding ((|||))

import           XMonad.Actions.ShowText

import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.FadeInactive
import           XMonad.Hooks.InsertPosition
import           XMonad.Hooks.ManageHelpers
import           XMonad.Hooks.ManageHelpers
import           XMonad.Hooks.UrgencyHook

import           XMonad.Util.NamedScratchpad
import           XMonad.Util.NamedWindows
import           XMonad.Util.Run

import           XMonad.Layout.Accordion
import           XMonad.Layout.BoringWindows     (boringWindows)
import           XMonad.Layout.Combo
import qualified XMonad.Layout.GridVariants      as GV
import           XMonad.Layout.LayoutCombinators
import           XMonad.Layout.LimitWindows
import           XMonad.Layout.Magnifier         (magnifiercz')
import           XMonad.Layout.Minimize
import           XMonad.Layout.MultiToggle
import           XMonad.Layout.Named
import           XMonad.Layout.NoBorders         (smartBorders)
import           XMonad.Layout.PerWorkspace      (onWorkspace)
import           XMonad.Layout.Reflect
import           XMonad.Layout.ResizableTile
import           XMonad.Layout.StackTile
import           XMonad.Layout.Tabbed
import           XMonad.Layout.TwoPane

import           Control.Monad
import           Data.Default                    (def)
import qualified Data.Map                        as M
import           Data.Monoid
import qualified XMonad.StackSet                 as W

import           XMonad.Util.PIPWindow           (pipManageHook)

import           Config
import           Utils

myLogHook :: X ()
myLogHook =
    dynamicLog <+>
    fadeInactiveLogHook fadeAmount
  where
    fadeAmount = 0.9

myManageHook :: ManageHook
myManageHook = composeAll $
    [ insertPosition Master Newer
    , manageHook def
    , pipManageHook
    , namedScratchpadManageHook myScratchpads
    , isDialog     --> doCenterFloat
    , isFullscreen --> doF W.focusDown <+> doFullFloat
    ] ++
    [ className ~? "slack"               --> doShift "speak"
    , appName   =? "xfce4-notifyd"       --> doIgnore
    , title     ~? "hangouts.google.com" --> doIgnore
    ] ++
    [ matchAny v --> doCenterFloat | v <- floatApps ]
  where
    -- Floating apps
    floatApps =
        [ "feh"
        , "font-manager"
        , "gcolor2"
        , "gifview"
        , "mpv"
        , "pinentry"
        , "sxiv"
        , "xmessage"
        , "zenity"
        , "Page Unresponsive"
        ]

myScratchpads :: [NamedScratchpad]
myScratchpads =
    [ termNS    "scratchpad"  "~"              (customFloating smallRect)
    , termAppNS "music"       "ncmpcpp"        (customFloating largeRect)
    , appNS     "colorpicker" "gcolor2"         doCenterFloat
    , appNS     "dictionary"  "goldendict"     (customFloating rightRect)
    , pidginNS  "contacts"    "buddy_list"     (customFloating sidebar)
    , pidginNS  "messages"    "conversation"    doCenterFloat
    , appNS     "volume"      "pavucontrol"     doCenterFloat
    , appNS     "wifi"        "wpa_gui"         doCenterFloat
    , termAppNS "bluetooth"   "bluetoothctl -a" doCenterFloat
    , emacsNS   "editor"                        (customFloating largeRect)
    ]
  where
    -- NS
    appNS      n c = NS n c (findClass c)
    termNS     n d = NS n (spawnTerm n d) (findRole n)
    termAppNS  n c = NS n (spawnTermApp n c) (findRole n)
    pidginNS   n r = NS n "pidgin" (findClassRole "pidgin" r)
    emacsNS    n   = NS n (spawnEmacs n) (findName n)
    emacsAppNS n c = NS n (spawnEmacsApp n c) (findName n)

    -- Commands
    spawnEmacs    n   = "emacsclient -c -F '((name . \"" ++ n ++ "\"))'"
    spawnEmacsApp n c = spawnEmacs n ++ " -e '(" ++ c ++ ")'"
    spawnTerm     r d = myTerm ++ " -r " ++ r ++ " -d " ++ d
    spawnTermApp  r c = myTerm ++ " -r " ++ r ++ " -e '" ++ c ++ "'"

    -- Finders
    findClass c = className ~? c
    findName n = wmName =? n
    findRole r = wmRole =? r
    findClassRole c r = className ~? c <&&> wmRole =? r

myLayoutHook =
    smartBorders $
    minimize $
    boringWindows $

    -- Mirror the layout in the X and Y axis.
    mkToggle (single REFLECTX) $
    mkToggle (single REFLECTY) $

    onWorkspace "speak" (Full ||| grid ||| dualStack ||| tabs) $

    grid ||| tabs ||| dualStack ||| accordionFull ||| magnifiercz' 1.4 triple ||| Full
  where
    tiled         = named "Tiled" (ResizableTall 1 delta (2/3) [])
    triple        = named "Triple" (limitWindows 3 tiled)
    tabs          = named "Tabbed" (tabbed shrinkText myTabConfig)
    accordion     = named "Folded" Accordion
    accordionFull = named "Folded Full" (combineTwo (TwoPane delta (1/2)) Accordion Full)
    dual          = named "Dual" (TwoPane delta (2/3))
    dualStack     = named "Dual Stacked" (combineTwo (StackTile 1 delta (1/2)) Full Full)
    grid          = named "Grid" (GV.SplitGrid GV.L 1 1 (2/3) (16/10) (5/100))
    delta = 3/100

myEventHook :: Event -> X All
myEventHook = handleTimerEvent

-- Queries
matchAny :: String -> Query Bool
matchAny x = foldr ((<||>) . (~? x)) (return False) [className, title, wmName, wmRole]

wmName, wmRole :: Query String
wmName = stringProperty "WM_NAME"
wmRole = stringProperty "WM_WINDOW_ROLE"

-- Floating window sizes (X, Y, width, height)
largeRect, smallRect, topRect, bottomRect, leftRect, rightRect :: W.RationalRect
largeRect  = W.RationalRect (1/20) (1/20) (18/20) (18/20)
smallRect  = W.RationalRect (1/6)  (1/6)  (4/6)   (4/6)
topRect    = W.RationalRect 0      0      1       (1/3)
bottomRect = W.RationalRect 0      (2/3)  1       (1/3)
leftRect   = W.RationalRect 0      0      (1/3)   1
rightRect  = W.RationalRect (2/3)  0      (1/3)   1
sidebar    = W.RationalRect 0      0      (1/10)  1

data LibNotifyUrgencyHook = LibNotifyUrgencyHook deriving (Read, Show)

instance UrgencyHook LibNotifyUrgencyHook where
    urgencyHook LibNotifyUrgencyHook w = do
        name     <- getName w
        Just idx <- fmap (W.findTag w) $ gets windowset
        safeSpawn "notify-send" [show name, "workspace " ++ idx]

myUrgencyHook = LibNotifyUrgencyHook
