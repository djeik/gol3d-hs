module App where

import Gol3d.Life hiding ( Position )

import Data.IORef
import Graphics.UI.GLUT

import Types

-- | Get the position of the cursor in the game with a given radius from
-- the camera position.
cursorLocation' :: GLfloat -> CamState -> Vector3 GLfloat
cursorLocation' r (CamState { camPos = Vector3 x y z
                            , camAngle = Vector2 theta phi
                            }) = Vector3 x' y' z'
    where x' = x + r * cos theta * sin phi
          y' = y + r * cos phi
          z' = z + r * sin theta * sin phi

-- | Get the position of the cursor in the game.
-- The game cursor is location a distance r away from the current camera
-- position in the direction of the camera orientation. Equally, this is
-- the forwards movement direction.
cursorLocation :: CamState -> Vector3 GLfloat
cursorLocation cs@(CamState { cursorRadius = r }) = cursorLocation' r cs

gameCursorLocation :: CamState -> Vector3 GLint
gameCursorLocation = fmap round . cursorLocation

-- | Transform the camera state advancing the camera by the given amount in
-- the current direction of the camera.
advanceCamera :: GLfloat -> CamState -> CamState
advanceCamera amt s = s { camPos = cursorLocation' amt s }

-- | Impurely transform the game state given as an "IORef" advancing the
-- camera by a given amount in the direction of the camera.
advanceCamera' :: IORef State -> GLfloat -> IO ()
advanceCamera' stateR k = do
    s <- readIORef stateR
    let cs' = advanceCamera (k * moveSpeed s) (camState s)
    writeIORef stateR (s { camState = cs' })

-- | Impurely transform the game state given as an "IORef" translating the
-- camera by the given vector.
-- Translations are applied relative to the current view as given by the
-- angles "theta" and "phi" in the "State".
transCamera' :: IORef State -> Vector2 GLfloat -> IO ()
transCamera' stateR (Vector2 dx dy) = do
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

-- | Transform a given "CamState" to adjust its angles according to a two
-- dimensional vector. The displacement (in pixels) represented by the vector
-- is multiplied by a scaling factor in radians per pixel.
adjustCameraAngle :: GLfloat -> Vector2 GLint -> CamState -> CamState
adjustCameraAngle aspd (Vector2 dx dy) s@(CamState { camAngle = Vector2 theta phi }) =
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

-- | Bring the "CellMap" contained by a "State" to the next generation.
-- This also updates the "lastEvolve" time of "State".
evolveState stateR = do
    s@(State { cellMap = cm
             , lastEvolve = le
             }) <- readIORef stateR
    et <- elapsedTime
    writeIORef stateR (s { cellMap = evolve cm
                         , lastEvolve = et
                         })

-- | Set the OpenGL projection to correspond with the given "CamState".
setCamera (CamState { camPos = Vector3 x y z
                    , camAngle = Vector2 theta phi
                    }) = do
    matrixMode $= Projection
    loadIdentity
    (_, Size w h) <- get viewport
    perspective 45.0 (fromIntegral w / fromIntegral h) 1.0 100.0

    matrixMode $= Modelview 0
    loadIdentity

    let offX = x + cos theta * sin phi
        offY = y + cos phi
        offZ = z + sin theta * sin phi

    let cast = fromRational . toRational

    lookAt (fmap cast $ Vertex3 x y z) (fmap cast $ Vertex3 offX offY offZ) $
        (Vector3 0 1 0)

-- | Switch between ViewMode and BuildMode
toggleMode :: GameMode -> GameMode
toggleMode BuildMode = ViewMode
toggleMode ViewMode = BuildMode
