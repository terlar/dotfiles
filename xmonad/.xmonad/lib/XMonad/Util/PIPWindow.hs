module XMonad.Util.PIPWindow (makePIPWindow, togglePIPWindow, pipManageHook) where

import Control.Monad
import Data.Maybe  (fromMaybe)
import Data.Monoid (Endo(..))

import XMonad
import XMonad.Operations (withFocused)
import qualified XMonad.StackSet as W

import XMonad.Util.WindowProperties (getProp32)

import XMonad.Actions.TagWindows
import XMonad.Actions.CopyWindow (copyToAll, killAllOtherCopies)

import XMonad.Hooks.ManageHelpers (doRectFloat)

import XMonad.Layout.Minimize
import qualified XMonad.Layout.BoringWindows as BW

makePIPWindow = withFocused $ \win -> do
    BW.markBoring
    setTags ["pip"] win
    resetPIPWindow win

resetPIPWindow :: Window -> X ()
resetPIPWindow win = do
    mh <- asks (manageHook . config)
    g <- appEndo <$> userCodeDef (Endo id) (runQuery mh win)
    windows g

tagValue :: Query String
tagValue = stringProperty "_XMONAD_TAGS"

togglePIPWindow = pipWindow togglePIPWindow'

togglePIPWindow' :: Window -> X ()
togglePIPWindow' win = withDisplay $ \d -> do
  wm_state <- getAtom "_NET_WM_STATE"
  mini <- getAtom "_NET_WM_STATE_HIDDEN"
  wstate <- fromMaybe [] `fmap` getProp32 wm_state win
  if fromIntegral mini `elem` wstate then
    showPIPWindow win
  else
    hidePIPWindow win

hidePIPWindow :: Window -> X ()
hidePIPWindow w = do
    focus w >> killAllOtherCopies
    minimizeWindow w

showPIPWindow :: Window -> X ()
showPIPWindow w = do
  sendMessage (RestoreMinimizedWin w)
  focus w >> windows W.shiftMaster

pipWindow = withTaggedGlobal "pip"

focusUpPIP   = focusUpTagged "pip"
focusDownPIP = focusDownTagged "pip"

pipManageHook = composeAll
    [ tagValue =? "pip" --> doRectFloat rect ]
  where
    rect = W.RationalRect (61/100) (2/100) (38/100) (38/100)
