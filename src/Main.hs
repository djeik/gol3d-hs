module Main where

-- We'd like to use OpenGL's Position unqualified
import Data.IORef
import Graphics.UI.GLUT

import Callbacks
import Types

main = do
    (progName, args) <- getArgsAndInitialize
    initialDisplayMode $= [DoubleBuffered, WithDepthBuffer]
    window <- createWindow "Gol3d"
    depthFunc $= Just Less
    cursor $= None
    globalKeyRepeat $= GlobalKeyRepeatOff

    stateR <- newIORef defaultState

    displayCallback $= display stateR
    keyboardMouseCallback $= Just (inputHandler stateR)
    passiveMotionCallback $= Just (motionHandler stateR)

    mainLoop
