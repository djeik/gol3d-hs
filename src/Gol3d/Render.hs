module Gol3d.Render
( drawCell
, drawCellMap
, CellDrawConfig(..)
, defaultCellDrawConfig
, defaultCursorDrawConfig
)
where

import Gol3d.Life

import Graphics.UI.GLUT

import Control.Monad ( forM_ )
import Data.Map ( elems )
import Data.Function ( on )
import Data.List ( sortBy )

data CellDrawConfig = CellDrawConfig { wireFrameColor :: Cell -> Color4 GLfloat
                                     , bodyColor :: Cell -> Color4 GLfloat
                                     , wireFrameFraction :: GLfloat
                                     , bodyFraction :: GLfloat
                                     }

-- | Compute the color of a "Cell", according to its age. New cells are
-- white, and become bluer as they age. If the age of a cell is less than
-- one, this function is undefined.
defaultBodyColor :: Cell -> Color4 GLfloat
defaultBodyColor (Cell { age = n } ) = Color4 (1-b) (1-b) 1 1
    where b = - 1 / fromIntegral n + 1

defaultCursorColor :: Cell -> Color4 GLfloat
defaultCursorColor = const $ Color4 0.2 0.8 0.2 0.75

-- | Constant black.
defaultWireFrameColor :: Cell -> Color4 GLfloat
defaultWireFrameColor = const $ Color4 0.5 0 0 0.8

defaultCursorWireFrameColor :: Cell -> Color4 GLfloat
defaultCursorWireFrameColor = const $ Color4 1.0 0 0 1.0


-- | The default configuration for drawing cells. The wireframe is simply
-- black, and the body of the cell becomes bluer as the cell ages.
-- The cell body will occupy 0.94^3 of the cell bounding box.
-- The wireframe will completely fill the bounding box.
defaultCellDrawConfig = CellDrawConfig { wireFrameColor = defaultWireFrameColor
                                       , bodyColor = defaultBodyColor
                                       , wireFrameFraction = 1.0
                                       , bodyFraction = 0.96
                                       }

defaultCursorDrawConfig = CellDrawConfig { wireFrameColor = defaultCursorWireFrameColor
                                         , bodyColor = defaultCursorColor
                                         , wireFrameFraction = 1.0
                                         , bodyFraction = 0.96
                                         }

-- | The offsets to add to a "Cell"'s position to determine its vertices.
-- The offsets are ordered such that the first 4 and the last 4 each define a
-- face in a respective z-plane. Furthermore, the pair of elements (n, n+4)
-- gives the segment joining corresponding vertices in these different planes.
boxOffsets :: [Vertex3 GLfloat]
boxOffsets = [ Vertex3 0 0 0
             , Vertex3 0 1 0
             , Vertex3 1 1 0
             , Vertex3 1 0 0
             , Vertex3 0 0 1
             , Vertex3 0 1 1
             , Vertex3 1 1 1
             , Vertex3 1 0 1
             ]

faceOffsets :: [[Vertex3 GLfloat]]
faceOffsets = [ [ Vertex3 0 0 0
                , Vertex3 0 1 0
                , Vertex3 1 1 0
                , Vertex3 1 0 0
                ]
              , [ Vertex3 0 0 1
                , Vertex3 0 1 1
                , Vertex3 1 1 1
                , Vertex3 1 0 1
                ]
              , [ Vertex3 0 0 0
                , Vertex3 0 0 1
                , Vertex3 1 0 1
                , Vertex3 1 0 0
                ]
              , [ Vertex3 0 1 0
                , Vertex3 0 1 1
                , Vertex3 1 1 1
                , Vertex3 1 1 0
                ]
              , [ Vertex3 0 0 0
                , Vertex3 0 0 1
                , Vertex3 0 1 1
                , Vertex3 0 1 0
                ]
              , [ Vertex3 1 0 0
                , Vertex3 1 0 1
                , Vertex3 1 1 1
                , Vertex3 1 1 0
                ]
              ]

-- | Zip a list with itself, dropping the first n elements.
-- Thus, each element is zipped with the one n spots later in the list.
selfZip :: Int -> [a] -> [(a, a)]
selfZip n xs = zip xs (drop n xs)

-- | Compute the coordinates of the vertices of the cell as it
-- should be rendered.
cellVertices :: GLfloat -> Cell
             -> [Vertex3 GLfloat] -> [Vertex3 GLfloat]
cellVertices k c offs = map ((+++) p . fmap scooch) offs
    where Vector3 x y z = cellPos c
          scooch l = l * k + (1-k)/2
          p = fmap fromIntegral $ Vertex3 x y z

-- | Add two "Vertex3"s component-wise. (Vector addition.)
(+++) :: Num a => Vertex3 a -> Vertex3 a -> Vertex3 a
Vertex3 x1 y1 z1 +++ Vertex3 x2 y2 z2 = Vertex3 (x1 + x2) (y1 + y2) (z1 + z2)

-- | Multiply a "Vertex3" by a scalar. (Scalar multiplication.)
(*/) :: Num a => Vertex3 a -> a -> Vertex3 a
Vertex3 x y z */ k = Vertex3 (x*k) (y*k) (z*k)

-- | For each component of the wire frame around the cell, compute its
-- center and the IO action required to draw it.
drawWireFrame :: GLfloat -- ^ fraction of each axis to fill
              -> Color4 GLfloat -- ^ color to draw the wireframe as
              -> Cell -- ^ cell whose wireframe to draw
              -> IO ()
drawWireFrame k col c = do
    color col
    -- Draw the loops around the two faces
    forM_ [take, drop] $ \f -> do
        renderPrimitive LineLoop . mapM_ vertex . f 4 $ vs
    -- Draw the lines that join the faces
    renderPrimitive Lines . mapM_ (both vertex) . selfZip 4 $ vs
    where vs = cellVertices k c boxOffsets
          both f (x1, x2) = (,) <$> f x1 <*> f x2

-- | For each face in the body, compute its center and the IO action
-- required to draw it.
drawBody :: GLfloat -- ^ fraction of each axis to fill
         -> Color4 GLfloat
         -> Cell -- ^ cell to draw
         -> [(Vertex3 GLfloat, IO ())]
         -- ^ association list of centers and drawing actions
drawBody k col c = map (\vs -> ( centerPoint 4 vs
                               , do color col
                                    renderPrimitive Quads (mapM_ vertex vs)
                               )
                       ) vss
    where vss = map (cellVertices k c) faceOffsets
          centerPoint :: GLfloat -> [Vertex3 GLfloat] -> Vertex3 GLfloat
          centerPoint n = fmap (/n) . foldr (+++) (Vertex3 0 0 0)

drawCell :: CellDrawConfig -> Vertex3 GLfloat -> Cell -> IO ()
drawCell cdf camPos c = do
    drawWireFrame (wireFrameFraction cdf) (wireFrameColor cdf c) c
    sequence_ . map snd $ sortBy (compare `on` (d . fst)) $ drawBody (bodyFraction cdf) (bodyColor cdf c) c
    where d p = let Vertex3 x y z = fmap (**2) $ camPos +++ (p */ (-1)) in x + y + z


drawCellMap :: CellDrawConfig -> Vertex3 GLfloat -> CellMap -> IO ()
drawCellMap cdf camPos = mapM_ (drawCell cdf camPos) . elems
