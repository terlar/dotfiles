module Utils where

import           XMonad

-- Data module
import           Data.Char (toUpper)
import           Data.List (isInfixOf)

import           Config

-- Infix (,) to clean up key and mouse bindings
infixr 0 ~>
(~>) :: a -> b -> (a, b)
(~>) = (,)

-- Query operators
needle `isInfixOfI` haystack = upper needle `isInfixOf` upper haystack
upper = map toUpper
q ~? x = fmap (x `isInfixOfI`) q -- haystack includes needle?

videoSelect :: X ()
videoSelect = spawnSelect myVideoPlayer "Select a video" "$XDG_VIDEOS_DIR"

pictureSelect :: X ()
pictureSelect = spawnSelect myImageViewer "Select a picture" "$XDG_PICTURES_DIR"

spawnSelect :: String -> String -> String -> X ()
spawnSelect cmd title path = spawn $
    cmd ++ " \"$(zenity --file-selection --title='" ++ title ++ "' --filename=" ++ path ++ "/)\""

restartXMonad :: MonadIO m => m ()
restartXMonad = spawn "xmonad --recompile && xmonad --restart"

programLauncher :: MonadIO m => m ()
programLauncher = spawn $ "x=$(yeganesh -x -- " ++ dmenuConfig ++ ") && exec $x"

passPrompt :: MonadIO m => m ()
passPrompt = spawn $ "x=$(passmenu " ++ dmenuConfig ++ ") && exec $x"
