-- | A handful of utility functions that don't really have a good place
-- anywhere else.
module Util where

import Graphics.UI.GLUT
import Types

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

-- | Compute (non-normalized) cartesian coordinates from unit spherical
-- coordinates.
sphericalToCartesian :: Vector2 GLfloat -> Vector3 GLfloat
sphericalToCartesian (Vector2 theta phi) = Vector3 x y z
    where x = cos theta * sin phi
          y = cos phi
          z = sin theta * sin phi

-- | Compute the vector that defines the direction of the camera's line of
-- sight.
lineOfSight :: CamState -> Vector3 GLfloat
lineOfSight = sphericalToCartesian . camAngle

vector3toVertex3 :: Vector3 a -> Vertex3 a
vector3toVertex3 (Vector3 x y z) = Vertex3 x y z
