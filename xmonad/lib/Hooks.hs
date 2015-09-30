module Hooks where

import XMonad hiding (Tall)
import XMonad.ManageHook
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.FadeInactive
import XMonad.Actions.CopyWindow
import XMonad.Util.Run
import XMonad.Util.NamedScratchpad

import XMonad.Layout.HintedTile
import XMonad.Layout.Reflect
import XMonad.Layout.Accordion
import XMonad.Layout.Renamed as R
import XMonad.Layout.MultiToggle
import XMonad.Layout.PerWorkspace (onWorkspace)
import XMonad.Layout.Magnifier (magnifiercz)
import XMonad.Layout.ResizableTile
import XMonad.Layout.TwoPane
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
    , appName =? "mplayer"       --> doCenterFloat
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

myLayoutHook = smartBorders $
    -- Mirror the layout in the X and Y axis.
    mkToggle (single REFLECTX) $
    mkToggle (single REFLECTY) $

    onWorkspace "speak" Full $

    tall ||| grid ||| magnifiercz 1.2 grid ||| dual ||| Full ||| fold
  where
    tall = renamed [R.Replace "Tile"] $
        ResizableTall 1 (2/100) (11/18) []
    grid = renamed [R.Replace "Grid"] $
        GV.SplitGrid GV.L 2 3 (2/3) (16/10) (5/100)
    dual = renamed [R.Replace "Dual"] $
        TwoPane (2/100) (1/2)
    fold = renamed [R.Replace "Fold"] Accordion
