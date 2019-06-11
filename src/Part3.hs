{-# LANGUAGE OverloadedStrings
  , BangPatterns
  , MultiWayIf
  , LambdaCase
  , TemplateHaskell
#-}
module Main where



import qualified Graphics.UI.FLTK.LowLevel.FL  as FL
import           Graphics.UI.FLTK.LowLevel.Fl_Types
import           Graphics.UI.FLTK.LowLevel.FLTKHS
import           Graphics.UI.FLTK.LowLevel.Fl_Enumerations

import           Data.Vector                    ( Vector )
import qualified Data.Vector                   as V
import qualified Data.Vector.Algorithms.Merge  as VM
import qualified Data.Vector.Generic.Mutable   as VM
import           Data.Array
import           Data.IORef
import           Data.Thyme.Clock
import           Data.Thyme.Time         hiding ( Vector )
import           Data.Char                      ( chr )

import           Control.Lens




data Vec = Vec {
  _vx :: !Double
  , _vy :: !Double
  , _vz :: !Double
  , _vw :: !Double
  } deriving (Eq, Ord)
makeLenses ''Vec

data Triangle = Triangle {
  _vec0 :: !Vec
  , _vec1 :: !Vec
  , _vec2 :: !Vec
  }
makeLenses '' Triangle

data ScreenTriangle = ScreenTriangle {
    stv0 :: Position
    , stv1 :: Position
    , stv2 :: Position
    , stvColor :: Color
    }

newtype Mesh = Mesh {
    mesh :: Vector Triangle
}


crossP :: Vec -> Vec -> Vec
crossP (Vec x1 y1 z1 _) (Vec x2 y2 z2 _) =
    Vec (y1 * z2 - z1 * y2) (z1 * x2 - x1 * z2) (x1 * y2 - y1 * x2) 1.0

dotP :: Vec -> Vec -> Double
dotP (Vec x1 y1 z1 _) (Vec x2 y2 z2 _) = x1 * x2 + y1 * y2 + z1 * z2

magnitude :: Vec -> Double
magnitude (Vec x y z _) = sqrt (x * x + y * y + z * z)

normalize :: Vec -> Vec
normalize v@(Vec x y z _) =
    let mag = magnitude v in Vec (x / mag) (y / mag) (z / mag) 1

instance Num Vec where
    Vec x1 y1 z1 _ + Vec x2 y2 z2 _ = Vec (x1 + x2) (y1 + y2) (z1 + z2) 1
    Vec x1 y1 z1 _ * Vec x2 y2 z2 _ = Vec (x1 * x2) (y1 * y2) (z1 * z2) 1
    Vec x1 y1 z1 _ - Vec x2 y2 z2 _ = Vec (x1 - x2) (y1 - y2) (z1 - z2) 1

    abs (Vec x y z w) = Vec (abs x) (abs y) (abs z) (abs w)
    signum (Vec x y z _) = if (x == 0) && (y == 0) && (z == 0) then 0 else 1

    fromInteger x = Vec (fromIntegral x) 0 0 1

normal :: Triangle -> Vec
normal tri =
    let line1 = tri ^. vec1 - tri ^. vec0
        line2 = tri ^. vec2 - tri ^. vec0
    in  normalize (crossP line1 line2)



drawScene :: Matrix -> SceneStateRef -> Ref Widget -> IO ()
drawScene matProj ref widget = do
    now <- getCurrentTime
    let fElapsedTime :: Double
        fElapsedTime = toSeconds (utctDayTime (unUTCTime now))
    scState    <- readIORef ref
    rectangle' <- getRectangle widget
    let coords@(x', y', w', h') = fromRectangle rectangle'
    flcPushClip rectangle'
    flcSetColor blackColor
    flcRectf rectangle'

    let fTheta  = scTheta scState + 1.0 * fElapsedTime
        matRotZ = matrixRotateZ (fTheta * 0.5)
        matRotX = matrixRotateX fTheta
        matTrans = matrixTranslate 0 0 16
        matWorld = matrixMultiplyMatrix
            (matrixMultiplyMatrix matRotZ matRotX) matTrans
        vCamera = scCamera scState

    let triTransformed :: Triangle -> Triangle
        triTransformed tri =
            tri
                & over vec0 (multMatrixVec matWorld)
                & over vec1 (multMatrixVec matWorld)
                & over vec2 (multMatrixVec matWorld)

        line1 tri =
            tri ^. vec1 - tri ^. vec0
        line2 tri =
            tri ^. vec2 - tri ^. vec0

        normal tri = normalize (crossP (line1 tri) (line2 tri))

        vCameraRay tri = tri ^. vec0 - vCamera

        discr tri =
            dotP (normal tri) (vCameraRay tri)


        draw tri = do
            let triTrans = triTransformed tri
            when (discr tri < 0) $ do
                let dp = max 0.1 (dotP light (normal tri))
                    color = rgbColorWithGrayscale . chr . Prelude.round $ abs dp * 255
                    triViewed = Triangle
                        (multMatrixVec matView (triTrans ^. vec0))
                        (multMatrixVec matView (triTrans ^. vec1))
                        (multMatrixVec matView (triTrans ^. vec2))

    --     triProjected :: Triangle -> Triangle
    --     triProjected tri =
    --         tri
    --             & over vec0 (multMatrixVec matProj)
    --             & over vec1 (multMatrixVec matProj)
    --             & over vec2 (multMatrixVec matProj)

    --     triDraw :: Triangle -> ScreenTriangle
    --     triDraw tri =
    --         let triangle =
    --                 triProjected tri
    --                     &  vec0
    --                     .  vx
    --                     +~ 1.0
    --                     &  vec0
    --                     .  vy
    --                     +~ 1.0
    --                     &  vec1
    --                     .  vx
    --                     +~ 1.0
    --                     &  vec1
    --                     .  vy
    --                     +~ 1.0
    --                     &  vec2
    --                     .  vx
    --                     +~ 1.0
    --                     &  vec2
    --                     .  vy
    --                     +~ 1.0
    --                     &  vec0
    --                     .  vx
    --                     *~ 0.5
    --                     *  fromIntegral w'
    --                     &  vec0
    --                     .  vy
    --                     *~ 0.5
    --                     *  fromIntegral h'
    --                     &  vec1
    --                     .  vx
    --                     *~ 0.5
    --                     *  fromIntegral w'
    --                     &  vec1
    --                     .  vy
    --                     *~ 0.5
    --                     *  fromIntegral h'
    --                     &  vec2
    --                     .  vx
    --                     *~ 0.5
    --                     *  fromIntegral w'
    --                     &  vec2
    --                     .  vy
    --                     *~ 0.5
    --                     *  fromIntegral h'
    --                     &  triToScreen
    --             triToScreen (Triangle v0 v1 v2) = ScreenTriangle
    --                 (vecToPosition v0)
    --                 (vecToPosition v1)
    --                 (vecToPosition v2)
    --                 blackColor -- will be filled in later
    --             vecToPosition (Vec x y _z _) =
    --                 Position (X (Prelude.round x)) (Y (Prelude.round y))
    --         in  triangle

    -- let vect :: Vector (Triangle, Color, Bool)
    --     vect = V.map (\tri ->
    --         let transTri = triTranslated tri
    --             n        = normal transTri
    --             doDraw   = triCulled transTri n
    --             lightDir = normalize light
    --             lum      = dotP n lightDir
    --             color    = rgbColorWithGrayscale . chr . Prelude.round $ abs lum * 255
    --         in
    --         (transTri, color, doDraw)) (mesh (scMesh scState))

    --     midpoints (t1, _, _) (t2, _, _)=
    --         let z1 = t1 ^. vec0 . vz + t1 ^. vec1 . vz + t1 ^. vec2 . vz / 3.0
    --             z2 = t2 ^. vec0 . vz + t2 ^. vec1 . vz + t2 ^. vec2 . vz / 3.0
    --         in compare z2 z1

    -- mv <- do
    --     v' <- V.thaw vect
    --     VM.sortBy midpoints v'
    --     V.unsafeFreeze v'


    -- V.forM_ mv $ \(tri, color, doDraw) ->
    --     if doDraw
    --         then do
    --             let dt = (triDraw tri) { stvColor = color }
    --             drawTriangle dt
    --         else pure ()

    flcPopClip


drawTriangle :: ScreenTriangle -> IO ()
drawTriangle (ScreenTriangle v0 v1 v2 color) = do
    flcSetColor color
    flcPolygon v0 v1 v2
    -- flcSetColor blackColor
    -- flcLoop v0 v1 v2



loadMesh :: FilePath -> IO Mesh
loadMesh file = do
    content <- readFile file
    let ls         = lines content
        !vertexVec = V.fromList (vertices ls)
        vertices []            = []
        vertices (line : rest) = case words line of
            "v" : x : y : z : _ ->
                Vec (read x) (read y) (read z) 1 : vertices rest
            _ -> vertices rest
        triangleVec = V.fromList (faces ls)
        faces []            = []
        faces (line : rest) = case words line of
            "f" : f0 : f1 : f2 : _ ->
                let v0 = vertexVec V.! ((read f0) - 1)
                    v1 = vertexVec V.! ((read f1) - 1)
                    v2 = vertexVec V.! ((read f2) - 1)
                in  Triangle v0 v1 v2 : faces rest
            _ -> faces rest
    return (Mesh triangleVec)



type Matrix = Array (Int, Int) Double

camera :: Vec
camera = Vec 0 0 0 1

light :: Vec
light = normalize (Vec 0 1 (-1) 1)



multMatrixVec :: Matrix -> Vec -> Vec
multMatrixVec !mat !i =
    let !ox =
            i^. vx *  mat !  (0, 0)
            +  i ^. vy * mat !  (1, 0)
            +  i ^. vz * mat !  (2, 0)
            +  i ^. vw * mat !  (3, 0)
        !oy =
            _vx i * mat ! (0, 1)
            + _vy i * mat ! (1, 1)
            + _vz i * mat ! (2, 1)
            + _vw i * mat ! (3, 1)
        !oz =
            _vx i * mat ! (0, 2)
            + _vy i * mat ! (1, 2)
            + _vz i * mat ! (2, 2)
            + _vw i * mat ! (3, 2)
        !ow =
            _vx i * mat ! (0, 3)
            + _vy i * mat ! (1, 3)
            + _vz i * mat ! (2, 3)
            + _vw i * mat ! (3, 3)
    in  Vec ox oy oz ow

identityMatrix :: Matrix
identityMatrix =
    array
    ((0, 0), (3, 3))
    [ if( x == y) then ((x, y), 1.0) else ((x, y), 0.0) | x <- [0..3], y <- [0..3]
    ]


matrixTranslate :: Double -> Double -> Double -> Matrix
matrixTranslate x y z =
    array
    ((0, 0), (3, 3))
    [ ((0, 0), 1)
    , ((0, 1), 0)
    , ((1, 0), 0)
    , ((1, 1), 1)
    , ((2, 2), 1)
    , ((3, 3), 1)
    , ((0, 2), 0)
    , ((0, 3), 0)
    , ((1, 2), 0)
    , ((1, 3), 0)
    , ((2, 0), 0)
    , ((2, 1), 0)
    , ((2, 3), 0)
    , ((3, 0), x)
    , ((3, 1), y)
    , ((3, 2), z)
    ]


matrixRotateZ :: Double -> Matrix
matrixRotateZ fTheta =
    array
    ((0, 0), (3, 3))
    [ ((0, 0), cos fTheta)
    , ((0, 1), sin fTheta)
    , ((1, 0), -sin fTheta)
    , ((1, 1), cos fTheta)
    , ((2, 2), 1)
    , ((3, 3), 1)
    , ((0, 2), 0)
    , ((0, 3), 0)
    , ((1, 2), 0)
    , ((1, 3), 0)
    , ((2, 0), 0)
    , ((2, 1), 0)
    , ((2, 3), 0)
    , ((3, 0), 0)
    , ((3, 1), 0)
    , ((3, 2), 0)
    ]


matrixRotateX :: Double -> Matrix
matrixRotateX fTheta =
    array
    ((0, 0), (3, 3))
    [ ((0, 0), 1)
    , ((1, 1), cos fTheta)
    , ((1, 2), sin fTheta)
    , ((2, 1), -sin fTheta)
    , ((2, 2), cos fTheta)
    , ((3, 3), 1)
    , ((0, 1), 0)
    , ((0, 2), 0)
    , ((0, 3), 0)
    , ((1, 0), 0)
    , ((1, 3), 0)
    , ((2, 0), 0)
    , ((2, 3), 0)
    , ((3, 0), 0)
    , ((3, 1), 0)
    , ((3, 2), 0)
    ]

matrixRotateY :: Double -> Matrix
matrixRotateY fTheta =
    array
    ((0, 0), (3, 3))
    [ ((0, 0), cos fTheta)
    , ((1, 1), 1)
    , ((1, 2), 0)
    , ((2, 1), 0)
    , ((2, 2), cos fTheta)
    , ((3, 3), 1)
    , ((0, 1), 0)
    , ((0, 2), sin fTheta)
    , ((0, 3), 0)
    , ((1, 0), 0)
    , ((1, 3), 0)
    , ((2, 0), sin fTheta)
    , ((2, 3), 0)
    , ((3, 0), 0)
    , ((3, 1), 0)
    , ((3, 2), 0)
    ]


projectionMatrix :: Double -> Double -> Double -> Double -> Matrix
projectionMatrix fAspectRatio fFovRad fFar fNear =
    array
    ((0, 0), (3, 3))
    [ ((0, 0), fAspectRatio * fFovRad)
    , ((0, 1), 0)
    , ((0, 2), 0)
    , ((0, 3), 0)
    , ((1, 1), fFovRad)
    , ((1, 0), 0)
    , ((1, 2), 0)
    , ((1, 3), 0)
    , ((2, 2), fFar / (fFar - fNear))
    , ((2, 0), 0)
    , ((2, 1), 0)
    , ((3, 2), (-fFar * fNear) / (fFar - fNear))
    , ((2, 3), 1.0)
    , ((3, 3), 0.0)
    , ((3, 0), 0.0)
    , ((3, 1), 0.0)
    ]


matrixMultiplyMatrix :: Matrix -> Matrix -> Matrix
matrixMultiplyMatrix m1 m2 =
    array ((0, 0), (3, 3))
    [ ((r, c), m1 ! (r, 0) * m2 ! (0, c) + m1 ! (r, 1) * m2 ! (1, c)
        + m1 ! (r, 2) * m2 ! (2, c) + m1 ! (r, 3) * m2 ! (3, c))
    | c <- [0..3], r <- [0..3]]





data SceneState = SceneState {
    scWidth :: Width
    , scHeight :: Height
    , scTheta :: Double
    , scMesh :: Mesh
    , scCamera :: Vec
    }


type SceneStateRef = IORef SceneState


main :: IO ()
main = do

    mesh <- loadMesh "VideoShip.obj"

    let fNear  = 0.1
        fFar   = 1000.0
        fFov   = 90.0
        width  = 800
        height = 600
        fAspectRatio :: Double
        fAspectRatio = fromIntegral height / fromIntegral width
        fFovRad      = 1.0 / tan (fFov * 0.5 / 180.0 * pi)

        matProj      = projectionMatrix fAspectRatio fFovRad fFar fNear

    ref     <- newIORef (SceneState (Width width) (Height height) 0.0 mesh camera)

    window' <- doubleWindowNew (Size (Width width) (Height height))
                               Nothing
                               Nothing
    begin window'
    widget' <- widgetCustom
        (Rectangle (Position (X 0) (Y 0)) (Size (Width width) (Height height)))
        Nothing
        (drawScene matProj ref)
        defaultCustomWidgetFuncs
    end window'
    showWidget window'

    doUntil (eventLoopEnds window' widget')
  where
    eventLoopEnds :: Ref DoubleWindow -> Ref Widget -> IO Bool
    eventLoopEnds window' widget' = do
        let update = do
                redraw widget'
            continue todo = if (todo == 0)
                then return True
                else do
                    update
                    return False
        isVisible <- getVisible (window')
        if isVisible then FL.check >>= continue else FL.wait >>= continue

    doUntil action = do
        shouldStop <- action
        if shouldStop then return () else doUntil action

