module XMonad.Util.NamedScratchpadBringer (bringNamedScratchpad) where

import           XMonad
import           XMonad.Util.NamedScratchpad

import           Control.Monad               (filterM)
import           Data.Maybe                  (listToMaybe)

import qualified XMonad.StackSet             as W

findByName :: NamedScratchpads -> String -> Maybe NamedScratchpad
findByName c s = listToMaybe $ filter ((s==) . name) c

bringNamedScratchpad :: NamedScratchpads
                     -> String
                     -> X ()
bringNamedScratchpad = bringSomeNamedScratchpad (\f ws -> f $ head ws)

bringSomeNamedScratchpad :: ((Window -> X ()) -> [Window] -> X ())
                         -> NamedScratchpads
                         -> String
                         -> X ()
bringSomeNamedScratchpad f confs n
    | Just conf <- findByName confs n = withWindowSet $ \s -> do
        filterCurrent <- filterM
                         (runQuery (query conf))
                         ((maybe [] W.integrate . W.stack . W.workspace . W.current) s)

        filterAll     <- filterM
                         (runQuery (query conf))
                         (W.allWindows s)

        case filterCurrent of
            [] -> do
                case filterAll of
                    [] -> return ()
                    _  -> f (windows . W.shiftWin (W.currentTag s)) filterAll
            _  -> do
                f (windows . W.focusWindow) filterAll
                windows W.shiftMaster
    | otherwise = return ()
