module Input where

import Control.Monad ( when )
import Data.IORef
import Data.Maybe ( fromMaybe )
import Graphics.UI.GLUT

import qualified Data.Map as M

import Types

-- | A pure version of "getKeyState". Rather than look up the key in the
-- global "State"'s "KeyboardState", the supplied "KeyboardState" is used
-- instead.
getKeyState' kbd key = fromMaybe Up $ M.lookup key kbd

-- | Get the state of a given key, checking in the global "State".
getKeyState :: IORef State -> Key -> IO KeyState
getKeyState stateR key = do
    s@(State { kbdState = kbd }) <- get stateR
    return $ getKeyState' kbd key

-- | A more pure version of "whenKey". Rather than checking the global
-- "State" for the key, a given "KeyboardState" is used.
whenKey' kbd key state action = when (getKeyState' kbd key == state) action

-- | Run a given action if a key has a certain state.
-- The key is checked in the global "State".
whenKey stateR key state action = do
    s <- getKeyState stateR key
    when (s == state) action
