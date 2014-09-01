module Utils where

import XMonad

import Config

role :: Query String
role = stringProperty "WM_WINDOW_ROLE"

videoSelect :: X ()
videoSelect = spawnSelect myVideoPlayer "Select a video" "$XDG_VIDEOS_DIR"

pdfSelect :: X ()
pdfSelect = spawnSelect myPDFViewer "Select a PDF" "$XDG_DOCUMENTS_DIR"

pictureSelect :: X ()
pictureSelect = spawnSelect myImageViewer "Select a picture" "$XDG_PICTURES_DIR"

spawnSelect :: String -> String -> String -> X ()
spawnSelect cmd title path = spawn $
    cmd ++ " \"$(zenity --file-selection --title='" ++ title ++ "' --filename=" ++ path ++ "/)\""

restartXMonad :: MonadIO m => m ()
restartXMonad = spawn "xmonad --recompile && xmonad --restart"

programLauncher :: MonadIO m => m ()
programLauncher = spawn $ "x=$(yeganesh -x -- " ++ dmenuConfig ++ ") && exec $x"
