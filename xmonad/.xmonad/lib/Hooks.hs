module Hooks where

import           XMonad                          hiding ((|||))
import           XMonad.ManageHook

import           XMonad.Actions.CopyWindow
import           XMonad.Actions.Promote          (promote)
import           XMonad.Actions.TagWindows
import           XMonad.Actions.UpdatePointer

import           XMonad.Hooks.DynamicLog

import           XMonad.Hooks.FadeInactive
import           XMonad.Hooks.InsertPosition
import           XMonad.Hooks.ManageHelpers

import           XMonad.Util.NamedScratchpad
import           XMonad.Util.Run

import           XMonad.Layout.Accordion
import           XMonad.Layout.BoringWindows     (boringWindows)
import           XMonad.Layout.Combo
import qualified XMonad.Layout.GridVariants      as GV
import           XMonad.Layout.HintedTile
import           XMonad.Layout.LayoutCombinators
import           XMonad.Layout.LayoutModifier
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

import qualified Data.Map                        as M
import qualified XMonad.StackSet                 as W

import           Data.Monoid

import           XMonad.Util.PIPWindow           (pipManageHook)

import           Config
import           Utils

myLogHook =
  dynamicLog <+>
  fadeInactiveLogHook fadeAmount
  where
    fadeAmount = 0.9

myManageHook :: ManageHook
myManageHook = composeAll $
  [ insertPosition Master Newer
  , manageHook defaultConfig
  , pipManageHook
  , namedScratchpadManageHook myScratchpads
  , isDialog     --> doCenterFloat
  , isFullscreen --> doF W.focusDown <+> doFullFloat
  ] ++
  [ appName =? "xfce4-notifyd"       --> doIgnore
  , title   ~? "hangouts.google.com" --> doIgnore
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
      , "sxiv"
      , "zenity"
      ]

myScratchpads =
  [ termNS    "scratchpad" "~"           (customFloating smallRect)
  , termAppNS "music"      "ncmpcpp"     (customFloating largeRect)
  , xAppNS    "volume"     "pavucontrol" doCenterFloat
  , xAppNS    "dictionary" "goldendict"  (customFloating rightRect)
  , emacsNS   "editor"                   (customFloating largeRect)
  ]
  where
    -- NS types
    termNS     n d = roleNS n (myTerm ++ " -r " ++ n ++ " -d " ++ d) n
    termAppNS  n c = roleNS n (myTerm ++ " -r " ++ n ++ " -e '" ++ c ++ "'") n
    xAppNS     n c = classNS n c c
    emacsNS    n   = nameNS n ("emacsclient -c -F '((name . \"" ++ n ++ "\"))'")
    emacsAppNS n c = nameNS n ("emacsclient -c -F '((name . \"" ++ n ++ "\"))' -e '" ++ c ++ "'")

    -- Query methods
    roleNS  n c r  = NS n c (role =? r)
    classNS n c cl = NS n c (className ~? cl)
    nameNS  n c    = NS n c (wmName =? n)

myLayoutHook =
  smartBorders $
  minimize $
  boringWindows $

  -- Mirror the layout in the X and Y axis.
  mkToggle (single REFLECTX) $
  mkToggle (single REFLECTY) $

  onWorkspace "web" (tabs ||| Full ||| dualStack) $
  onWorkspace "speak" (Full ||| tiled ||| dualStack ||| tabs) $

  tiled ||| grid ||| tabs ||| dualStack ||| accordionFull ||| magnifiercz' 1.4 triple ||| Full
  where
    tiled = named "Tiled" (ResizableTall 1 delta (2/3) [])
    triple = named "Triple" (limitWindows 3 tiled)
    tabs = named "Tabbed" (tabbed shrinkText myTabConfig)
    accordion = named "Folded" Accordion
    accordionFull = named "Folded Full" (combineTwo (TwoPane delta (1/2)) Accordion Full)
    dual = named "Dual" (TwoPane delta (2/3))
    dualStack = named "Dual Stacked" (combineTwo (StackTile 1 delta (1/2)) Full Full)
    grid = named "Grid" (GV.SplitGrid GV.L 2 3 (2/3) (16/10) (5/100))
    delta = 3/100

myEventHook = floatClickFocusHandler

-- Bring clicked floating window to the front
floatClickFocusHandler :: Event -> X All
floatClickFocusHandler ButtonEvent{ev_window = w} = withWindowSet $ \s ->
  do
    if isFloat w s
      then (focus w >> promote)
      else return ()

    return (All True)
    where
      isFloat w ss = M.member w $ W.floating ss
floatClickFocusHandler _ = return (All True)

-- Queries
matchAny :: String -> Query Bool
matchAny x = foldr ((<||>) . (=? x)) (return False) [className, title, wmName, role]

wmName :: Query String
wmName = stringProperty "WM_NAME"

role :: Query String
role = stringProperty "WM_WINDOW_ROLE"

-- Floating window sizes (X, Y, width, height)
largeRect  = W.RationalRect (1/20) (1/20) (18/20) (18/20)
smallRect  = W.RationalRect (1/6)  (1/6)  (4/6)   (4/6)
topRect    = W.RationalRect 0      0      1       (1/3)
bottomRect = W.RationalRect 0      (2/3)  1       (1/3)
leftRect   = W.RationalRect 0      0      (1/3)   1
rightRect  = W.RationalRect (2/3)  0      (1/3)   1
