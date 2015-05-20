module Callbacks where

import Gol3d.Render
import Gol3d.Life

import Control.Monad ( when )
import Data.IORef
import Graphics.UI.GLUT
import System.Exit ( exitWith, ExitCode(ExitSuccess) )

import qualified Data.Map as M

import Util
import Input
import Types
import App

keyPollMap :: IORef State -> [(Key, IO ())]
keyPollMap stateR = [ (Char 'w', advanceCamera' stateR 1)
                    , (Char 'a', transCamera' stateR (Vector2 (-1) 0))
                    , (Char 'd', transCamera' stateR (Vector2 1 0))
                    , (Char 's', advanceCamera' stateR (-1))
                    , (Char 'q', transCamera' stateR (Vector2 0 (-1)))
                    , (Char 'e', transCamera' stateR (Vector2 0 1))
                    ]

-- | An input handler that uses polling.
keyPollHandler stateR = do
    s@(State { kbdState = kbd }) <- get stateR
    let whenDown key action = whenKey' kbd key Down $
                                  do action
                                     postRedisplay Nothing

    mapM_ (uncurry whenDown) (keyPollMap stateR)

    et <- get elapsedTime
    s <- get stateR
    stateR $= s { lastKeyPoll = et }

inputHandler stateR key state mods pos = do
    s@(State { kbdState = kbd
             , gameMode = mode
             , isPlaying = playing
             }) <- get stateR
    let kbd' = M.insert key state kbd

    let mode' = case (state, key) of
                    (Down, Char 'b') -> toggleMode mode
                    _ -> mode

    let isPlaying' = case (state, key) of
                         (Down, Char 'p') -> not playing
                         _ -> playing

    writeIORef stateR $ s { kbdState = kbd'
                          , gameMode = mode'
                          , isPlaying = isPlaying'
                          }

    case state of
        Down ->
            case key of
                Char ' ' -> evolveState stateR
                Char 'x' -> exitWith ExitSuccess
                _ -> return ()
        _ -> return ()

    postRedisplay Nothing

mouseHandler stateR btn keyState pos = do
    s@(State { camState = cs
             , gameMode = mode
             , cellMap = cm
             }) <- get stateR

    let (f, act) = case keyState of
                       Down ->
                           ( case btn of
                               LeftButton -> insertCell' $
                                             fmap fromIntegral $
                                             gameCursorLocation cs
                               RightButton -> deleteCell' $
                                              fmap fromIntegral $
                                              gameCursorLocation cs
                               _ -> id
                           , postRedisplay Nothing
                           )
                       _ -> (id, return ())

    writeIORef stateR (s { cellMap = f cm })
    act

motionHandler stateR p = do
    s@(State { camState = cs
             , angleSpeed = aspd
             }) <- get stateR
    d@(dx, dy) <- mouseDelta p
    when (dx /= 0 || dy /= 0) $ do
        let cs' = adjustCameraAngle aspd (Vector2 dx dy) cs
        writeIORef stateR (s { camState = cs' })
        centerMouse
        postRedisplay Nothing

idleHandler stateR = do
    s@(State { lastEvolve = le
             , evolveDelta = ed
             , lastKeyPoll = lkp
             , keyPollDelta = kpd
             , isPlaying = ip
             }) <- get stateR

    et <- get elapsedTime

    when (et >= lkp + kpd) $ keyPollHandler stateR

    when (ip && et >= le + ed) $ evolveState stateR

display :: IORef State -> DisplayCallback
display stateR = do
    s@(State { cellDrawConfig = celldc
             , cursorDrawConfig = cursordc
             , camState = cs
             , cellMap = cm
             , evolveDelta = ed
             , lastEvolve = le
             , gameMode = mode
             }) <- get stateR

    clear [ColorBuffer, DepthBuffer]
    setCamera cs

    blend $= Disabled
    drawCellMap celldc (vector3toVertex3 $ camPos cs) cm

    blend $= Enabled
    when (mode == BuildMode) $ do
        blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
        drawCell cursordc (vector3toVertex3 $ camPos cs) $
            newCell $ fmap fromIntegral $ gameCursorLocation cs

    swapBuffers
