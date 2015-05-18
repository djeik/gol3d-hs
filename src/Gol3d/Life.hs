module Gol3d.Life
( Vector3
, Cell(..)
, CellMap, CellSet, CellList
, Ruleset
, toCellMap
, standardRuleset
, evolveWith, evolve
, newCell
)
where

import Graphics.UI.GLUT ( Vector3(..) )

import qualified Data.Map.Strict as M
import qualified Data.Set as S

import Data.Maybe ( mapMaybe )

data SurvivalResult = Birth | Survival | Death
                    deriving (Show, Eq, Ord)

type NeighborCount = Int
type Ruleset = NeighborCount -> SurvivalResult

type Position = Vector3 Int

data Cell = Cell { age :: Int
                 , cellPos :: Position
                 }
                 deriving (Show, Read, Eq)

type NeighborCountMatrix = M.Map Position NeighborCount

type CellMap = M.Map Position Cell
type CellSet = S.Set Cell
type CellList = [Cell]

-- | The offsets to give to a position to compute its neighbors.
offsets :: [Position]
offsets = [ Vector3 x y z | x <- [-1..1], y <- [-1..1], z <- [-1..1],
            (x, y, z) /= (0, 0, 0)
          ]

-- | The neighboring points of a given position.
neighbors :: Position -> [Position]
neighbors p = map (+++ p) offsets
    where Vector3 x1 x2 x3 +++ Vector3 y1 y2 y3 =
            Vector3 (x1 + y1) (x2 + y2) (x3 + y3)

-- | Build a new cell (with age 1) at a given location.
newCell :: Position -> Cell
newCell p = Cell { age = 1, cellPos = p }

-- | Increment the age of a given cell.
incCell :: Cell -> Cell
incCell c = c { age = age c + 1 }

-- | Convert a "CellList" to a "CellMap".
toCellMap :: CellList -> CellMap
toCellMap = M.fromList . map (\c -> (cellPos c, c))

toCellList :: CellMap -> CellList
toCellList = M.elems

-- | Build a "NeighborCountMatrix" from a list of "Cell"s.
makeNCM :: CellList -> NeighborCountMatrix
makeNCM = foldr (f . neighbors . cellPos) M.empty
    where f ps m = foldr g m ps
          g p m = case M.lookup p m of
                      Nothing -> M.insert p 1 m
                      Just n -> M.insert p (n + 1) m

-- | The standard ruleset for Game of Life 3D.
--
-- Cells are born exactly when there are six neighbors.
-- Cells survive when they have between 5 and 7 neighbors.
-- Otherwise, cells die.
standardRuleset n
    | n <= birthMax && n >= birthMin = Birth
    | n <= liveMax && n >= liveMin = Survival
    | otherwise = Death
    where birthMin = 6
          birthMax = 6
          liveMin = 5
          liveMax = 7

-- | Compute the next generation of a "CellMap" with a given "Ruleset".
evolveWith :: Ruleset -> CellMap -> CellMap
evolveWith rs cm = toCellMap $ mapMaybe f $ M.assocs ncm
    where ncm = makeNCM $ M.elems cm
          f (p, count) = case rs count of
                             Death -> Nothing
                             Birth -> case M.lookup p cm of
                                          Nothing -> Just $ newCell p
                                          Just c -> Just $ incCell c
                             Survival -> case M.lookup p cm of
                                             Nothing -> Nothing
                                             Just c -> Just $ incCell c

-- | Compute the next generaton of a "CellMap" with the "standardRuleset".
evolve = evolveWith standardRuleset
