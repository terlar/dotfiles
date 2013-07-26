import XMonad

main = xmonad defaultConfig
  { modMask            = mod4Mask
  , normalBorderColor  = "#dddddd"
  , focusedBorderColor = "#3d4962"
  , terminal           = "termite"
  }
