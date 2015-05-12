module Callbacks where

import Gol3d.Render

import Control.Monad ( when )
import Data.IORef
import Graphics.UI.GLUT
import System.Exit ( exitWith, ExitCode(ExitSuccess) )

import qualified Data.Map as M

import Input
import Types
import App

inputHandler stateR key state mods pos = do
    s@(State { kbdState = kbd }) <- get stateR
    let kbd' = M.insert key state kbd
    writeIORef stateR $ s { kbdState = kbd' }

    case state of
        Down ->
            case key of
                Char ' ' -> evolveState stateR
                Char 'x' -> exitWith ExitSuccess
                _ -> return ()
        _ -> return ()

motionHandler stateR p = do
    s@(State { camState = cs
             , angleSpeed = aspd
             }) <- get stateR
    d@(dx, dy) <- mouseDelta p
    when (dx /= 0 || dy /= 0) $ do
        let cs' = adjustCameraAngle aspd dx dy cs
        writeIORef stateR (s { camState = cs' })
        centerMouse

display stateR = do
    keyPollHandler stateR

    s@(State { cellDrawConfig = cdc
             , camState = cs
             , cellMap = cm
             , evolveDelta = ed
             , lastEvolve = le
             }) <- readIORef stateR

    et <- elapsedTime
    when (et >= le + ed) $ evolveState stateR

    clear [ColorBuffer, DepthBuffer]
    setCamera cs
    drawCellMap cdc cm
    swapBuffers
    postRedisplay Nothing -- TODO put this in the input handler oslt
