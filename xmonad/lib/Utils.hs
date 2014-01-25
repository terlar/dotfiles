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
videoSelect = spawn $ myVideoPlayer ++ " \"$(zenity --file-selection --title=\"Select a video\" --filename=$XDG_VIDEOS_DIR/)\""

pdfSelect :: X ()
pdfSelect = spawn $ myPdfViewer ++ " \"$(zenity --file-selection --title=\"Select a pdf\" --filename=$XDG_DOCUMENTS_DIR/)\""

pictureSelect :: X ()
pictureSelect = spawn $ myImageViewer ++ " \"$(zenity --file-selection --title=\"Select a Picture\" --filename=$XDG_PICTURES_DIR/)\""

restartXMonad :: MonadIO m => m ()
restartXMonad = spawn $ "xmonad --recompile && xmonad --restart"

programLauncher :: MonadIO m => m ()
programLauncher = spawn $ "x=$(yeganesh -x -- " ++ myDmenuOptions ++ ") && exec $x"
