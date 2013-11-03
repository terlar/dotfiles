module Utils
     ( videoSelect
     , pdfSelect
     , pictureSelect
     , restartXMonad
     , programLauncher
     ) where

import XMonad

import Config

videoSelect :: X ()
videoSelect = spawn $ myVideoPlayer ++ " \"$(zenity --file-selection --title=\"Select a video\" --filename=$HOME/Video/)\""

pdfSelect :: X ()
pdfSelect = spawn $ myPdfViewer ++ " \"$(zenity --file-selection --title=\"Select a pdf\" --filename=$HOME/Books/)\""

pictureSelect :: X ()
pictureSelect = spawn $ myImageViewer ++ " \"$(zenity --file-selection --title=\"Select a Picture\" --filename=$HOME/Pictures/)\""

restartXMonad :: MonadIO m => m ()
restartXMonad = spawn $ "xmonad --recompile && xmonad --restart"

programLauncher :: MonadIO m => m ()
programLauncher = spawn $ "x=$(yeganesh -x -- " ++ myDmenuOptions ++ ") && exec $x"
