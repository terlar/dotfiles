module XMonad.Util.PIPWindow (makePIPWindow, togglePIPWindow, pipManageHook) where

import Control.Monad
import Data.Maybe  (fromMaybe)
import Data.Monoid (Endo(..))

import XMonad
import XMonad.Operations (withFocused)
import qualified XMonad.StackSet as W

import XMonad.Util.WindowProperties (getProp32)

import XMonad.Actions.CopyWindow (copyToAll, killAllOtherCopies)

import XMonad.Hooks.ManageHelpers (doRectFloat)

import XMonad.Layout.Minimize
import qualified XMonad.Layout.BoringWindows as BW

makePIPWindow = withFocused $ \win -> do
    setRole win "pip"
    BW.markBoring
    mh <- asks (manageHook . config)
    g <- appEndo <$> userCodeDef (Endo id) (runQuery mh win)
    windows g

atom_WM_WINDOW_ROLE :: X Atom
atom_WM_WINDOW_ROLE = getAtom "WM_WINDOW_ROLE"

setRole :: Window -> String -> X ()
setRole w v = withDisplay $ \dpy -> do
    a <- atom_WM_WINDOW_ROLE
    io $ setTextProperty dpy w v a

roleName :: Query String
roleName = stringProperty "WM_WINDOW_ROLE"

togglePIPWindow = pipWindow togglePIPWindow'

togglePIPWindow' :: Window -> X ()
togglePIPWindow' win = do
  wm_state <- getAtom "_NET_WM_STATE"
  mini <- getAtom "_NET_WM_STATE_HIDDEN"
  wstate <- fromMaybe [] `fmap` getProp32 wm_state win
  if fromIntegral mini `elem` wstate then
    sendMessage $ RestoreMinimizedWin win
  else
    minimizeWindow win

pipWindow f = pipWindow' (mapM_ f)

pipWindow' :: ([Window] -> X ()) -> X ()
pipWindow' m = gets windowset >>=
    filterM (runQuery (roleName =? "pip")) . concat . map (W.integrate' . W.stack) . W.workspaces >>= m

pipManageHook = composeAll
    [ roleName =? "pip" --> doRectFloat rect ]
  where
    rect = W.RationalRect (61/100) (2/100) (38/100) (38/100)
