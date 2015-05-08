module Main where

import Gol3d.Life hiding ( Position )
import Gol3d.Pattern
import Gol3d.Render

import Control.Monad ( when )
import Data.IORef
import Graphics.UI.GLUT
import System.Exit ( exitWith, ExitCode(..) )

import qualified Data.Map as M

data CamState = CamState { camPos :: Vector3 GLfloat
                         , camAngle :: Vector2 GLfloat
                         }

data State = State { cellDrawConfig :: CellDrawConfig
                   -- ^ The configuration used to draw the "Cell"s in the
                   -- stored "CellMap".
                   , camState :: CamState
                   -- ^ The current state of the camera.
                   , cellMap :: CellMap
                   -- ^ The current map of cells.
                   , evolveDelta :: Int
                   -- ^ The time between "CellMap" evolutions.
                   , lastEvolve :: Int
                   -- ^ The time of the last evolution.
                   , moveSpeed :: GLfloat
                   -- ^ The radius of the vision sphere
                   , angleSpeed :: GLfloat
                   -- ^ radians per pixel
                   }

defaultState = State { cellDrawConfig = defaultCellDrawConfig
                     , camState = CamState { camPos = Vector3 0 0 0
                                           , camAngle = Vector2 0 0
                                           }
                     , cellMap = toCellMap glider3
                     , evolveDelta = 100000000
                     , lastEvolve = 0
                     , moveSpeed = 0.1
                     , angleSpeed = 0.005
                     }

display stateR = do
    s@(State { cellDrawConfig = cdc
           , camState = cs
           , cellMap = cm
           , evolveDelta = ed
           , lastEvolve = le
           }) <- readIORef stateR

    et <- elapsedTime
    when (et >= le + ed) $ writeIORef stateR (s { cellMap = evolve cm
                                                , lastEvolve = et
                                                })

    clear [ColorBuffer, DepthBuffer]
    setCamera cs
    drawCellMap cdc cm
    swapBuffers
    postRedisplay Nothing -- TODO put this in the input handler oslt

advanceCamera' stateR k = do
    s <- readIORef stateR
    let cs' = advanceCamera (k * moveSpeed s) (camState s)
    writeIORef stateR (s { camState = cs' })

motionHandler stateR p = do
    s@(State { camState = cs
             , angleSpeed = aspd
             }) <- get stateR
    d@(dx, dy) <- mouseDelta p
    when (dx /= 0 || dy /= 0) $ do
        print d
        let cs' = adjustCameraAngle aspd dx dy cs
        writeIORef stateR (s { camState = cs' })
        centerMouse

adjustCameraAngle aspd dx dy s@(CamState { camAngle = Vector2 theta phi }) =
    s { camAngle = Vector2 theta' phi' }
    where theta' = between 0 tau $ theta + aspd * fromIntegral dx
          phi' = between 0 pi $ phi + aspd * fromIntegral dy
          tau = 2 * pi
          between lo hi x
              | x < lo = lo
              | x > hi = hi
              | otherwise = x

inputHandler stateR (Char 'w') Down _ _ = advanceCamera' stateR 1
inputHandler stateR (Char 's') Down _ _ = advanceCamera' stateR (-1)
inputHandler _ (Char 'q') Down _ _ = exitWith ExitSuccess
inputHandler _ _ _ _ _ = return ()

-- | Warps the mouse to the center of the viewport.
centerMouse :: IO ()
centerMouse = do
    (_, Size w h) <- get viewport
    pointerPosition $= Position (w `div` 2) (h `div` 2)
    putStrLn "warped mouse"

-- | Computes how far the mouse is from the center of the viewport.
-- Uses the OpenGL convention that the origin is the bottom *left* corner of
-- the window. This is contrary to GLUT (and most window managers) that assume
-- that the origin is the top right corner of the window.
mouseDelta :: Position -> IO (GLint, GLint)
mouseDelta (Position x y) = do
    (_, Size w h) <- get viewport
    return (x - (w `div` 2), y - (h `div` 2))

setCamera (CamState { camPos = Vector3 x y z
                    , camAngle = Vector2 theta phi
                    }) = do
    matrixMode $= Projection
    loadIdentity
    perspective 45.0 1 1.0 50.0
    -- frustum (-5) 5 (-5) 5 1.0 40.0

    matrixMode $= Modelview 0
    loadIdentity

    let offX = x + cos theta * sin phi
        offY = y + cos phi
        offZ = z + sin theta * sin phi

    let cast = fromRational . toRational

    lookAt (fmap cast $ Vertex3 x y z) (fmap cast $ Vertex3 offX offY offZ) $
        (Vector3 0 1 0)

advanceCamera :: GLfloat -> CamState -> CamState
advanceCamera amt s@(CamState { camPos = Vector3 x y z
                              , camAngle = Vector2 theta phi
                              }) = s { camPos = Vector3 offX offY offZ }
    where offX = x + amt * cos theta * sin phi
          offY = y + amt * cos phi
          offZ = z + amt * sin theta * sin phi

main = do
    (progName, args) <- getArgsAndInitialize
    initialDisplayMode $= [DoubleBuffered, WithDepthBuffer]
    window <- createWindow "Gol3d"
    depthFunc $= Just Less
    cursor $= None

    stateR <- newIORef defaultState

    displayCallback $= display stateR
    keyboardMouseCallback $= Just (inputHandler stateR)
    passiveMotionCallback $= Just (motionHandler stateR)

    mainLoop