module Hooks where

import XMonad hiding ((|||))
import XMonad.ManageHook
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.FadeInactive
import XMonad.Actions.CopyWindow
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
import XMonad.Layout.NoBorders (smartBorders)

import qualified XMonad.StackSet as W
import qualified XMonad.Layout.GridVariants as GV

import Config
import Utils

myHooks =
    myManageHook <+>
    namedScratchpadManageHook myScratchpads

myLogHook = fadeInactiveLogHook fadeAmount
  where fadeAmount = 0.9

myManageHook = composeAll
    [ manageHook defaultConfig
    , isFullscreen               --> doFullFloat
    , isDialog                   --> doCenterFloat
    , appName =? "lxappearance"  --> doCenterFloat
    , appName =? "qtconfig-qt4"  --> doCenterFloat
    , appName =? "mpv"           --> doCenterFloat
    , appName =? "sxiv"          --> doCenterFloat
    , appName =? "feh"           --> doCenterFloat
    , appName =? "gifview"       --> doCenterFloat
    , appName =? "zenity"        --> doCenterFloat
    , appName =? "gcolor2"       --> doCenterFloat
    , appName =? "font-manager"  --> doCenterFloat
    , appName =? "xfce4-notifyd" --> doIgnore
    , appName =? "spotify"       --> doShift "music"
    ]

myScratchpads =
    [ workPad "scratchpad" "~" smallRect
    , termPad "ncmpcpp" largeRect
    , xappPad "pavucontrol" "Pavucontrol" doCenterFloat
    , xappPad "goldendict" "Goldendict" doRightFloat
    ]
  where
    workPad a d = NS a (workTerm a d) (role ~? a)
    workTerm a d = myTerm ++ " -r " ++ a ++ " -d " ++ d
    termPad a = NS a (consoleApp a) (role ~? a)
    consoleApp a = myTerm ++ " -r " ++ a ++ " -e \"" ++ a ++ "\""
    xappPad a c = NS a a (className ~? c)

    -- Floating window sizes
    largeRect = customFloating $ W.RationalRect (1/20) (1/20) (9/10) (9/10)
    smallRect = customFloating $ W.RationalRect (1/6) (1/6) (2/3) (2/3)
    doLeftFloat = customFloating $ W.RationalRect 0 0 (1/3) 1
    doRightFloat = customFloating $ W.RationalRect (2/3) 0 (1/3) 1

myLayoutHook = smartBorders $
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
    accordion = named "Folded" (Accordion)
    accordionFull = named "Folded Full" (combineTwo (TwoPane delta (1/2)) (Accordion) (Full))
    dual = named "Dual" (TwoPane delta (2/3))
    dualStack = named "Dual Stacked" (combineTwo (StackTile 1 delta (1/2)) (Full) (Full))
    grid = named "Grid" (GV.SplitGrid GV.L 2 3 (2/3) (16/10) (5/100))
    delta = 3/100
