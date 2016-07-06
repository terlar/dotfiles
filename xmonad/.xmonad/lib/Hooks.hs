module Hooks where

import XMonad hiding ((|||))
import XMonad.ManageHook

import XMonad.Actions.CopyWindow
import XMonad.Actions.TagWindows

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.FadeInactive

import XMonad.Util.Run
import XMonad.Util.NamedScratchpad

import XMonad.Layout.HintedTile
import XMonad.Layout.Reflect
import XMonad.Layout.Accordion
import XMonad.Layout.Named
import XMonad.Layout.Combo
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.MultiToggle
import XMonad.Layout.PerWorkspace (onWorkspace)
import XMonad.Layout.Magnifier (magnifiercz')
import XMonad.Layout.ResizableTile
import XMonad.Layout.TwoPane
import XMonad.Layout.StackTile
import XMonad.Layout.LimitWindows
import XMonad.Layout.Tabbed
import XMonad.Layout.Minimize
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Layout.BoringWindows (boringWindows)
import XMonad.Layout.LayoutModifier
import qualified XMonad.Layout.GridVariants as GV

import qualified XMonad.StackSet as W

import XMonad.Util.PIPWindow (pipManageHook)

import Config
import Utils

myLogHook = dynamicLog <+> fadeInactiveLogHook fadeAmount
  where
    fadeAmount = 0.9

myManageHook :: ManageHook
myManageHook = composeAll
    [ manageHook defaultConfig
    , pipManageHook
    , namedScratchpadManageHook myScratchpads
    , isFullscreen                      --> doF W.focusDown <+> doFullFloat
    , isDialog                          --> doCenterFloat
    , appName  =? "lxappearance"        --> doCenterFloat
    , appName  =? "qtconfig-qt4"        --> doCenterFloat
    , appName  =? "mpv"                 --> doCenterFloat
    , appName  =? "sxiv"                --> doCenterFloat
    , appName  =? "feh"                 --> doCenterFloat
    , appName  =? "gifview"             --> doCenterFloat
    , appName  =? "zenity"              --> doCenterFloat
    , appName  =? "gcolor2"             --> doCenterFloat
    , appName  =? "font-manager"        --> doCenterFloat
    , appName  =? "emacs"               --> smallRect
    , appName  =? "spotify"             --> doShift "music"
    , appName  =? "xfce4-notifyd"       --> doIgnore
    , title    ~? "hangouts.google.com" --> doIgnore
    , fmap not isDialog                 --> doF avoidMaster
    ]
  where
    avoidMaster :: W.StackSet i l a s sd -> W.StackSet i l a s sd
    avoidMaster = W.modify' $ \c -> case c of
        W.Stack t [] (r:rs) ->  W.Stack r [] (t:rs)
        otherwise           -> c
    -- Floating window sizes
    largeRect = customFloating $ W.RationalRect (1/20) (1/20) (9/10) (9/10)
    smallRect = customFloating $ W.RationalRect (1/6) (1/6) (2/3) (2/3)

myScratchpads =
    [ termNS    "scratchpad" "~"           smallRect
    , termAppNS "music"      "ncmpcpp"     largeRect
    , xAppNS    "volume"     "pavucontrol" doCenterFloat
    , xAppNS    "dictionary" "goldendict"  doRightFloat
    , emacsNS   "editor"                   largeRect
    ]
  where
    -- NS types
    termNS     n d = roleNS n (myTerm ++ " -r " ++ n ++ " -d " ++ d) n
    termAppNS  n c = roleNS n (myTerm ++ " -r " ++ n ++ " -e '" ++ c ++ "'") n
    xAppNS     n c = classNS n c c
    emacsNS    n   = nameNS n ("emacsclient -c -F '((name . \"" ++ n ++ "\"))'")
    emacsAppNS n c = nameNS n ("emacsclient -c -F '((name . \"" ++ n ++ "\"))' -e '" ++ c ++ "'")

    -- Query methods
    roleNS  n c r  = NS n c (roleName =? r)
    classNS n c cl = NS n c (className ~? cl)
    nameNS  n c    = NS n c (wmName =? n)
    wmName         = stringProperty "WM_NAME"

    -- Floating window sizes
    largeRect    = customFloating $ W.RationalRect (1/20) (1/20) (9/10) (9/10)
    smallRect    = customFloating $ W.RationalRect (1/6)  (1/6)  (2/3)  (2/3)
    doLeftFloat  = customFloating $ W.RationalRect 0      0      (1/3)  1
    doRightFloat = customFloating $ W.RationalRect (2/3)  0      (1/3)  1

myLayoutHook = smartBorders $
    minimize $
    -- Mirror the layout in the X and Y axis.
    mkToggle (single REFLECTX) $
    mkToggle (single REFLECTY) $

    -- Boring windows
    boringWindows $

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
