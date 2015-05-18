module Types where

import Gol3d.Life hiding ( Position )
import Gol3d.Pattern
import Gol3d.Render

import Data.IORef
import Graphics.UI.GLUT

import qualified Data.Map as M

data CamState = CamState { camPos :: Vector3 GLfloat
                         , camAngle :: Vector2 GLfloat
                         , cursorRadius :: GLfloat
                         }

-- | An internally managed cache of key states.
-- We need this since GLUT doesn't give us a way to poll key states.
type KeyboardState = M.Map Key KeyState

data State = State { cellDrawConfig :: CellDrawConfig
                   -- ^ The configuration used to draw the "Cell"s in the
                   -- stored "CellMap".
                   , cursorDrawConfig :: CellDrawConfig
                   -- ^ The configuration used to draw the cursor.
                   , camState :: CamState
                   -- ^ The current state of the camera.
                   , kbdState :: KeyboardState
                   -- ^ An internally managed cache of key states.
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
                   , gameMode :: GameMode
                   }

defaultState = State { cellDrawConfig = defaultCellDrawConfig
                     , cursorDrawConfig = defaultCursorDrawConfig
                     , camState = CamState { camPos = Vector3 0 0 0
                                           , camAngle = Vector2 0 0
                                           , cursorRadius = 5.0
                                           }
                     , kbdState = M.empty
                     , cellMap = toCellMap glider3
                     , evolveDelta = 100000000
                     , lastEvolve = 0
                     , moveSpeed = 0.1
                     , angleSpeed = 0.005
                     , gameMode = BuildMode
                     }

data GameMode = BuildMode | ViewMode
              deriving (Eq, Ord, Show, Read)

type StateR = IORef State
