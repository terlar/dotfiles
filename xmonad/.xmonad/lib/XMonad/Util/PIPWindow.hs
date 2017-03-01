module XMonad.Util.PIPWindow ( makePIPWindow
                             , togglePIPWindow
                             , resetPIPWindow
                             , focusUpPIP
                             , focusDownPIP
                             , pipManageHook
                             , pipWindow
                             ) where

import           Control.Monad                    (filterM)
import           Data.Maybe                       (fromMaybe)
import           Data.Monoid                      (Endo (..))

import           XMonad
import           XMonad.Operations                (withFocused)
import qualified XMonad.StackSet                  as W

import           XMonad.Actions.CopyWindow        (copyToAll,
                                                   killAllOtherCopies)
import           XMonad.Actions.DynamicWorkspaces (addHiddenWorkspace)
import           XMonad.Actions.TagWindows

import           XMonad.Hooks.ManageHelpers       (doRectFloat)

import qualified XMonad.Layout.BoringWindows      as BW

tagValue :: Query String
tagValue = stringProperty "_XMONAD_TAGS"

pipQuery :: Query Bool
pipQuery = tagValue =? "pip"

pipManageHook :: Query (Endo WindowSet)
pipManageHook = composeAll
    [ pipQuery --> doRectFloat rect ]
  where
    rect = W.RationalRect (61/100) (2/100) (38/100) (38/100)

pipWindow :: (Window -> X ()) -> X ()
pipWindow = withTaggedGlobal "pip"

makePIPWindow :: X ()
makePIPWindow = withFocused $ \win -> do
    BW.markBoring
    setTags ["pip"] win
    resetPIPWindow win
    focus win >> windows copyToAll

togglePIPWindow :: X ()
togglePIPWindow = withWindowSet $ \s -> do
    filterCurrent <- filterM (runQuery pipQuery)
                    ( (maybe [] W.integrate . W.stack . W.workspace . W.current) s)

    case filterCurrent of
        (x:_) -> do
            -- create hidden workspace if it doesn't exist
            if null (filter ((== scratchpadWorkspaceTag) . W.tag) (W.workspaces s))
                then addHiddenWorkspace scratchpadWorkspaceTag
                else return ()
            -- kill copies of window
            focus x >> killAllOtherCopies
            -- push window there
            windows $ W.shiftWin scratchpadWorkspaceTag x
        [] -> do
            -- try to find it on all workspaces
            filterAll <- filterM (runQuery pipQuery) (W.allWindows s)
            case filterAll of
                (x:_) -> do
                    windows $ W.shiftWin (W.currentTag s) x
                    focus x >> BW.markBoring >> windows copyToAll
                [] -> return ()

-- Utilize same workspace tag as scratchpad
scratchpadWorkspaceTag :: String
scratchpadWorkspaceTag = "NSP"

resetPIPWindow :: Window -> X ()
resetPIPWindow win = do
    mh <- asks (manageHook . config)
    g  <- appEndo <$> userCodeDef (Endo id) (runQuery mh win)
    windows g

focusUpPIP, focusDownPIP :: X ()
focusUpPIP = focusUpTagged "pip"
focusDownPIP = focusDownTagged "pip"
