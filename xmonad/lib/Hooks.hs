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
import XMonad.Layout.MultiToggle
import XMonad.Layout.PerWorkspace (onWorkspace)
import XMonad.Layout.Magnifier (magnifiercz)
import XMonad.Layout.NoBorders (smartBorders)

import qualified XMonad.StackSet as W
import qualified XMonad.Layout.GridVariants as GV

import Config
import Utils

myManageHook :: ManageHook
myManageHook = (composeAll . concat $
    [ [ className =? "Xfce4-notifyd" --> doIgnore ]
    , [ className =? "Spotify" --> doShift "music" ]
    , [ className =? x --> doCenterFloat | x <- floats ]
    , [ isDialog --> doCenterFloat ]
    , [ isFullscreen --> doFullFloat ]
    ]) <+> namedScratchpadManageHook myScratchpads
  where
    floats =
        [ "MPlayer"
        , "Sxiv"
        , "feh"
        , "Gifview"
        , "Zenity"
        , "Gcolor2"
        , "Arandr"
        , "Lxappearance"
        , "Qtconfig-qt4"
        ]

myScratchpads :: NamedScratchpads
myScratchpads =
    [ NS "scratchpad" spawnTermSP findTerm manageSP
    , NS "volume" spawnVol findVol manageSP
    , NS "music" spawnMusic findMusic manageSP
    ]
  where
    spawnTermSP = myTerminal ++ " -r scratchpad"
    findTerm = role =? "scratchpad"

    spawnVol = "pavucontrol"
    findVol = className =? "Pavucontrol"

    spawnMusic = myTerminal ++ " -r music -e ncmpcpp"
    findMusic = role =? "music"

    manageSP = customFloating $ W.RationalRect l t w h
      where
        h = 0.7
        w = 0.7
        t = (1 - h) / 2
        l = (1 - w) / 2

myLogHook :: X ()
myLogHook = fadeInactiveLogHook fadeAmount
  where fadeAmount = 0.9

myLayoutHook =
    smartBorders $

    -- Mirror the layout in the X and Y axis.
    mkToggle (single REFLECTX) $
    mkToggle (single REFLECTY) $

    onWorkspace "speak" Full $

    grid ||| magnifiercz 1.2 grid ||| tall ||| wide ||| Full
  where
    grid  = GV.SplitGrid GV.L 2 3 (2/3) (16/10) (5/100)
    wide  = hintedTile (9/12) Wide
    tall  = hintedTile (1/2) Tall

    hintedTile r = HintedTile nmaster delta r Center

    -- The default number of windows in the master pane
    nmaster = 1
    -- Default proportion of screen occupied by master pane
    ratio   = 1/2
    -- Percent of screen to increment by when resizing panes
    delta   = 3/100
