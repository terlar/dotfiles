module Utils where

import           XMonad    (MonadIO, X, spawn)

-- Data module
import           Data.Char (toUpper)
import           Data.List (isInfixOf)

import           Config

-- Infix (,) to clean up key and mouse bindings
infixr 0 ~>
(~>) :: a -> b -> (a, b)
(~>) = (,)

-- Case insensitive haystack includes needle?
q ~? x = fmap (x `isInfixOfI`) q

isInfixOfI :: String -> String -> Bool
needle `isInfixOfI` haystack = upper needle `isInfixOf` upper haystack

upper :: String -> String
upper = map toUpper

-- Spawn functions
spawnSelect :: String -> String -> String -> X ()
spawnSelect cmd title path =
    spawn $ cmd ++ " \"$(zenity\
                   \ --file-selection\
                   \ --title='" ++ title ++ "'\
                   \ --filename=" ++ path ++ "/)\""

videoSelect, pictureSelect :: X ()
videoSelect   = spawnSelect myVideoPlayer "Select a video" "$XDG_VIDEOS_DIR"
pictureSelect = spawnSelect myImageViewer "Select a picture" "$XDG_PICTURES_DIR"

restartXMonad :: MonadIO m => m ()
restartXMonad = spawn "xmonad --recompile && xmonad --restart"

programLauncher :: MonadIO m => m ()
programLauncher = spawn "rofi -show run"

passPrompt :: MonadIO m => m ()
passPrompt = spawn "autopass"

searchPrompt :: MonadIO m => m ()
searchPrompt = spawn "rofi-surfraw"

calcPrompt :: MonadIO m => m ()
calcPrompt = spawn "menu-calc"
