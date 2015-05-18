module Main where

-- We'd like to use OpenGL's Position unqualified
import Data.IORef
import Graphics.UI.GLUT

import Callbacks
import Types

main = do
    (progName, args) <- getArgsAndInitialize

    initialDisplayMode $= [RGBAMode, WithAlphaComponent, DoubleBuffered, WithDepthBuffer]
    window <- createWindow "Gol3d"

    depthFunc $= Just Lequal
    cursor $= None
    globalKeyRepeat $= GlobalKeyRepeatOff

    stateR <- newIORef defaultState

    idleCallback $= Just (idleHandler stateR)
    keyboardMouseCallback $= Just (inputHandler stateR)
    mouseCallback $= Just (mouseHandler stateR)
    passiveMotionCallback $= Just (motionHandler stateR)

    displayCallback $= display stateR

    mainLoop
