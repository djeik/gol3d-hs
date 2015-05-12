module App where

import Gol3d.Life hiding ( Position )

import Data.IORef
import Graphics.UI.GLUT

import Types

-- | Transform the camera state advancing the camera by the given amount in
-- the current direction of the camera.
advanceCamera :: GLfloat -> CamState -> CamState
advanceCamera amt s@(CamState { camPos = Vector3 x y z
                              , camAngle = Vector2 theta phi
                              }) = s { camPos = Vector3 offX offY offZ }
    where offX = x + amt * cos theta * sin phi
          offY = y + amt * cos phi
          offZ = z + amt * sin theta * sin phi

-- | Impurely transform the game state given as an "IORef" advancing the
-- camera by a given amount in the direction of the camera.
advanceCamera' stateR k = do
    s <- readIORef stateR
    let cs' = advanceCamera (k * moveSpeed s) (camState s)
    writeIORef stateR (s { camState = cs' })

-- | Impurely transform the game state given as an "IORef" translating the
-- camera by the given vector. 
-- Translations are applied relative to the current view as given by the
-- angles "theta" and "phi" in the "State".
transCamera' stateR (dx, dy) = do
    s@(State { camState = cs@(CamState { camPos = Vector3 x y z
                                       , camAngle = Vector2 theta phi
                                       })
             , moveSpeed = spd
             }) <- get stateR

    let rho = theta + (pi/2)
        x' = x + spd * dx * cos rho
        y' = y + spd * dy
        z' = z + spd * dx * sin rho
        cs' = cs { camPos = Vector3 x' y' z' }

    writeIORef stateR (s { camState = cs' })

adjustCameraAngle aspd dx dy s@(CamState { camAngle = Vector2 theta phi }) =
    s { camAngle = Vector2 theta' phi' }
    where theta' = between 0 tau tau $ theta + aspd * fromIntegral dx
          phi' = boundedBy 0 pi $ phi + aspd * fromIntegral dy
          tau = 2 * pi
          boundedBy lo hi x
              | x < lo = lo
              | x > hi = hi
              | otherwise = x
          between lo hi adj x
              | x < lo = between lo hi adj (x + adj)
              | x > hi = between lo hi adj (x - adj)
              | otherwise = x

evolveState stateR = do
    putStrLn "evolving"
    s@(State { cellMap = cm
             , lastEvolve = le
             }) <- readIORef stateR
    et <- elapsedTime
    writeIORef stateR (s { cellMap = evolve cm
                         , lastEvolve = et
                         })

setCamera (CamState { camPos = Vector3 x y z
                    , camAngle = Vector2 theta phi
                    }) = do
    matrixMode $= Projection
    loadIdentity
    perspective 45.0 1 1.0 50.0

    matrixMode $= Modelview 0
    loadIdentity

    let offX = x + cos theta * sin phi
        offY = y + cos phi
        offZ = z + sin theta * sin phi

    let cast = fromRational . toRational

    lookAt (fmap cast $ Vertex3 x y z) (fmap cast $ Vertex3 offX offY offZ) $
        (Vector3 0 1 0)
