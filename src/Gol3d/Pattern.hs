module Gol3d.Pattern where

import Gol3d.Life

import Graphics.UI.GLUT

import qualified Data.Map as M

import Data.List ( intercalate )

glider3 :: CellList
glider3 = map newCell [ Vector3 2 1 0
                      , Vector3 3 2 0
                      , Vector3 1 3 0
                      , Vector3 2 3 0
                      , Vector3 3 3 0
                      , Vector3 2 1 1
                      , Vector3 3 2 1
                      , Vector3 1 3 1
                      , Vector3 2 3 1
                      , Vector3 3 3 1
                      ]

boring :: CellList
boring = [newCell $ Vector3 0 0 0 ]

toGol3dPattern :: CellList -> String
toGol3dPattern = intercalate "\n" . map showC
    where showC (Cell { cellPos = Vector3 x y z, age = n }) = intercalate " " $
            map show [x, y, z, n]
