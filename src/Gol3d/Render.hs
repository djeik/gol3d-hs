module Gol3d.Render
( drawCell
, drawCellMap
, CellDrawConfig(..)
, defaultCellDrawConfig
)
where

import Gol3d.Life

import Graphics.UI.GLUT

import Control.Monad ( forM_ )
import Data.Map ( elems )

data CellDrawConfig = CellDrawConfig { wireFrameColor :: Cell -> Color3 GLfloat
                                     , bodyColor :: Cell -> Color3 GLfloat
                                     , wireFrameFraction :: GLfloat
                                     , bodyFraction :: GLfloat
                                     }

-- | Compute the color of a "Cell", according to its age. New cells are
-- white, and become bluer as they age. If the age of a cell is less than
-- one, this function is undefined.
defaultBodyColor :: Cell -> Color3 GLfloat
defaultBodyColor (Cell { age = n } ) = Color3 (1-b) (1-b) 1
    where b = - 1 / fromIntegral n + 1

-- | Constant black.
defaultWireFrameColor :: Cell -> Color3 GLfloat
defaultWireFrameColor = const $ Color3 1.0 0 0

-- | The default configuration for drawing cells. The wireframe is simply
-- black, and the body of the cell becomes bluer as the cell ages.
-- The cell body will occupy 0.94^3 of the cell bounding box.
-- The wireframe will completely fill the bounding box.
defaultCellDrawConfig = CellDrawConfig { wireFrameColor = defaultWireFrameColor
                                       , bodyColor = defaultBodyColor
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

-- | Draw the wireframe around the cell.
drawWireFrame :: GLfloat -> Color3 GLfloat -> Cell -> IO ()
drawWireFrame k col c = do
    color col
    -- Draw the loops around the two faces
    forM_ [take, drop] $ \f -> do
        renderPrimitive LineLoop . mapM_ vertex . f 4 $ vs
    -- Draw the lines that join the faces
    renderPrimitive Lines . mapM_ (both vertex) . selfZip 4 $ vs
    where vs = cellVertices k c boxOffsets
          both f (x1, x2) = (,) <$> f x1 <*> f x2

drawBody :: GLfloat -> Color3 GLfloat -> Cell -> IO ()
drawBody k col c = do
    color col
    let verts = concat vss
    -- putStrLn "draw body"
    -- forM_ vss $ putStrLn . show . map (\(Vertex3 x y z) -> (x, y, z))
    renderPrimitive Quads . mapM_ vertex $ verts
    where vss = map (cellVertices k c) faceOffsets

drawCell :: CellDrawConfig -> Cell -> IO ()
drawCell cdf c = do
    drawWireFrame (wireFrameFraction cdf) (wireFrameColor cdf c) c
    drawBody (bodyFraction cdf) (bodyColor cdf c) c

drawCellMap :: CellDrawConfig -> CellMap -> IO ()
drawCellMap cdf = mapM_ (drawCell cdf) . elems
