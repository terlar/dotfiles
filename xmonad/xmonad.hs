import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.NoBorders
import XMonad.Util.EZConfig

main :: IO ()
main = do
  status <- statusBar 'xmobar'
    defaultPP
      { ppHidden  = pad
      , ppCurrent = xmobarColor "#ffffff" "#2b4f98" . pad
      , ppTitle   = pad . xmobarColor "#bbb" ""
      , ppLayout  = const ""
      , ppSep     = " "
      }
    toggleStrutsKey
    $ withUrgencyHook NoUrgencyHook
    $ defaultConfig
      { modMask            = mod4Mask
      , normalBorderColor  = "#dddddd"
      , focusedBorderColor = "#3d4962"
      , terminal           = "termite"
      , manageHook = composeAll
        [ isFullscreen            --> doFullFloat
        , isDialog                --> doCenterFloat
        , className =? "feh"      --> doCenterFloat
        , className =? "Chromium" --> doShift "2"
        , manageHook defaultConfig
        ]
      , layoutHook = smartBorders $ layoutHook defaultConfig
      }
      `additionalKeysP`
        [ ("M-p", spawn "x=$(yeganesh -x -- $DMENU_OPTIONS) && exec $x" ) -- Launcher
        , ("M-q", spawn "xmonad --recompile && xmonad --restart"        ) -- Restart xmonad
        , ("M-u", focusUrgent                                           ) -- Focus urgent WS
        , ("M1-l", spawn "i3lock"                                       ) -- Lock screen
        , ("<XF86AudioPlay>"       , spawn "mpc toggle"                 ) -- Play/pause
        , ("<XF86AudioStop>"       , spawn "mpc stop"                   ) -- Stop
        , ("<XF86AudioLowerVolume>", spawn "amixer -q set Master 5%-"   ) -- Decrease volume
        , ("<XF86AudioRaiseVolume>", spawn "amixer -q set Master 5%+"   ) -- Increase volume
        , ("<XF86AudioMute>"       , spawn "amixer -q set Master toggle") -- Mute volume
        ]

  xmonad status

toggleStrutsKey :: XConfig 1 -> (KeyMask, KeySym)
toggleStrutsKey XConfig { modMask = modm } = (modm, xK_b)
