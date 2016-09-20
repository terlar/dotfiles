module XMonad.Util.PIPWindow (makePIPWindow, togglePIPWindow, pipManageHook) where

import           Data.Maybe                   (fromMaybe)
import           Data.Monoid                  (Endo (..))

import           XMonad
import           XMonad.Operations            (withFocused)
import qualified XMonad.StackSet              as W

import           XMonad.Util.WindowProperties (getProp32)

import           XMonad.Actions.CopyWindow    (killAllOtherCopies)
import           XMonad.Actions.TagWindows

import           XMonad.Hooks.ManageHelpers   (doRectFloat)

import qualified XMonad.Layout.BoringWindows  as BW
import           XMonad.Layout.Minimize

makePIPWindow :: X ()
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

togglePIPWindow :: X ()
togglePIPWindow = pipWindow togglePIPWindow'

togglePIPWindow' :: Window -> X ()
togglePIPWindow' win = do
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

pipWindow :: (Window -> X ()) -> X ()
pipWindow = withTaggedGlobal "pip"

focusUpPIP :: X ()
focusUpPIP   = focusUpTagged "pip"
focusDownPIP :: X ()
focusDownPIP = focusDownTagged "pip"

pipManageHook :: Query (Endo WindowSet)
pipManageHook = composeAll
    [ tagValue =? "pip" --> doRectFloat rect ]
  where
    rect = W.RationalRect (61/100) (2/100) (38/100) (38/100)
