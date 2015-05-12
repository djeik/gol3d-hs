module Input where

import Control.Monad ( when )
import Data.IORef
import Data.Maybe ( fromMaybe )
import Graphics.UI.GLUT

import qualified Data.Map as M

import Types
import App

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

-- | An input handler that uses polling.
keyPollHandler stateR = do
    s@(State { kbdState = kbd }) <- get stateR
    let whenDown key action = whenKey' kbd key Down action

    whenDown (Char 'w') $ advanceCamera' stateR 1
    whenDown (Char 'a') $ transCamera' stateR (-1, 0)
    whenDown (Char 'd') $ transCamera' stateR (1, 0)
    whenDown (Char 's') $ advanceCamera' stateR (-1)
    whenDown (Char 'q') $ transCamera' stateR (0, -1)
    whenDown (Char 'e') $ transCamera' stateR (0, 1)

-- | Warps the mouse to the center of the viewport.
centerMouse :: IO ()
centerMouse = do
    (_, Size w h) <- get viewport
    pointerPosition $= Position (w `div` 2) (h `div` 2)

-- | Computes how far the mouse is from the center of the viewport.
-- Uses the OpenGL convention that the origin is the bottom *left* corner of
-- the window. This is contrary to GLUT (and most window managers) that assume
-- that the origin is the top right corner of the window.
mouseDelta :: Position -> IO (GLint, GLint)
mouseDelta (Position x y) = do
    (_, Size w h) <- get viewport
    return (x - (w `div` 2), y - (h `div` 2))

