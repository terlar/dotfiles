-- Base
import XMonad
import System.IO

-- Utilities
import XMonad.Util.EZConfig

-- Hooks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook

-- Actions
import XMonad.Actions.CycleRecentWS

-- Layouts
import XMonad.Layout.NoBorders

main = do
  xmonad $ withUrgencyHook NoUrgencyHook $ defaultConfig
    { modMask            = mod4Mask
    , normalBorderColor  = "#dddddd"
    , focusedBorderColor = "#3d4962"
    , terminal           = "termite"
    , manageHook = composeAll
      [ isFullscreen                --> doFullFloat
      , isDialog                    --> doCenterFloat
      , className =? "feh"          --> doCenterFloat
      , className =? "Pavucontrol"  --> doCenterFloat
      , manageHook defaultConfig
      ]
    , layoutHook = smartBorders $ layoutHook defaultConfig
    } `additionalKeysP`
    [ ("M-p", spawn "x=$(yeganesh -x -- $DMENU_OPTIONS) && exec $x"   ) -- Launcher
    , ("M-q", spawn "xmonad --recompile && xmonad --restart"          ) -- Restart xmonad
    , ("M-<Tab>", cycleRecentWS [xK_Super_L] xK_Tab xK_Tab            ) -- Cycle recent WS
    , ("M-u", focusUrgent                                             ) -- Focus urgent WS
    , ("M-<Esc>" , spawn "i3lock -i ~/Pictures/saltside.png -c 000000") -- Lock screen
    -- Media Keys
    , ("<XF86MonBrightnessUp>"  , spawn "xbacklight -inc 10"          ) -- Brighness up
    , ("<XF86MonBrightnessDown>", spawn "xbacklight -dec 10"          ) -- Brighness down
    , ("<XF86AudioPlay>"        , spawn "ncmpcpp toggle"              ) -- Play/Pause track
    , ("<XF86AudioStop>"        , spawn "ncmpcpp stop"                ) -- Stop track
    , ("<XF86AudioNext>"        , spawn "ncmpcpp next"                ) -- Next track
    , ("<XF86AudioPrev>"        , spawn "ncmpcpp prev"                ) -- Previous track
    , ("<XF86AudioLowerVolume>" , spawn "amixer -q set Master 5%-"    ) -- Decrease volume
    , ("<XF86AudioRaiseVolume>" , spawn "amixer -q set Master 5%+"    ) -- Increase volume
    , ("<XF86AudioMute>"        , spawn "amixer -q set Master toggle" ) -- Mute volume
    ]