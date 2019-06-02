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
import           Data.Array
import           Data.IORef
import           Data.Thyme.Clock
import           Data.Thyme.Time         hiding ( Vector )

import           Control.Lens


data Vec = Vec {
  _vx :: !Double
  , _vy :: !Double
  , _vz :: !Double
  } deriving (Eq, Ord)
makeLenses ''Vec

data Triangle = Triangle {
  _vec0 :: !Vec
  , _vec1 :: !Vec
  , _vec2 :: !Vec
  }
makeLenses '' Triangle

data ScreenTriangle = ScreenTriangle Position Position Position


crossP :: Vec -> Vec -> Vec
crossP (Vec x1 y1 z1) (Vec x2 y2 z2) =
    Vec (y1 * z2 - z1 * y2) (z1 * x2 - x1 * z2) (x1 * y2 - y1 * x2)

dotP :: Vec -> Vec -> Double
dotP (Vec x1 y1 z1) (Vec x2 y2 z2) = x1 * x2 + y1 * y2 + z1 * z2

magnitude :: Vec -> Double
magnitude (Vec x y z) = sqrt (x * x + y * y + z * z)

normalize :: Vec -> Vec
normalize v@(Vec x y z) =
    let mag = magnitude v in Vec (x / mag) (y / mag) (z / mag)

instance Num Vec where
    Vec x1 y1 z1 + Vec x2 y2 z2 = Vec (x1 + x2) (y1 + y2) (z1 + z2)
    Vec x1 y1 z1 * Vec x2 y2 z2 = Vec (x1 * x2) (y1 * y2) (z1 * z2)
    Vec x1 y1 z1 - Vec x2 y2 z2 = Vec (x1 - x2) (y1 - y2) (z1 - z2)

    abs (Vec x y z) = Vec (abs x) (abs y) (abs z)
    signum (Vec x y z) = if (x == 0) && (y == 0) && (z == 0) then 0 else 1

    fromInteger x = Vec (fromIntegral x) 0 0

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
        matRotZ = array
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

        matRotX = array
            ((0, 0), (3, 3))
            [ ((0, 0), 1)
            , ((1, 1), cos (fTheta * 0.5))
            , ((1, 2), sin (fTheta * 0.5))
            , ((2, 1), -sin (fTheta * 0.5))
            , ((2, 2), cos (fTheta * 0.5))
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


    let triRotatedZ :: Triangle -> Triangle
        triRotatedZ tri =
            tri
                & over vec0 (multMatrixVec matRotZ)
                & over vec1 (multMatrixVec matRotZ)
                & over vec2 (multMatrixVec matRotZ)
        triRotatedXZ :: Triangle -> Triangle
        triRotatedXZ tri =
            triRotatedZ tri
                & over vec0 (multMatrixVec matRotX)
                & over vec1 (multMatrixVec matRotX)
                & over vec2 (multMatrixVec matRotX)
        triTranslated :: Triangle -> Triangle
        triTranslated tri =
            triRotatedXZ tri
                & over (vec0 . vz) (+ 3.0)
                & over (vec1 . vz) (+ 3.0)
                & over (vec2 . vz) (+ 3.0)

        triCulled :: Triangle -> Vec -> Bool
        triCulled tri n = dotP n (tri ^. vec0 - camera) < 0

        triProjected :: Triangle -> Triangle
        triProjected tri =
            tri
                & over vec0 (multMatrixVec matProj)
                & over vec1 (multMatrixVec matProj)
                & over vec2 (multMatrixVec matProj)

        triDraw :: Triangle -> ScreenTriangle
        triDraw tri =
            let triangle =
                        triProjected tri
                            &  vec0
                            .  vx
                            +~ 1.0
                            &  vec0
                            .  vy
                            +~ 1.0
                            &  vec1
                            .  vx
                            +~ 1.0
                            &  vec1
                            .  vy
                            +~ 1.0
                            &  vec2
                            .  vx
                            +~ 1.0
                            &  vec2
                            .  vy
                            +~ 1.0
                            &  vec0
                            .  vx
                            *~ 0.5
                            *  fromIntegral w'
                            &  vec0
                            .  vy
                            *~ 0.5
                            *  fromIntegral h'
                            &  vec1
                            .  vx
                            *~ 0.5
                            *  fromIntegral w'
                            &  vec1
                            .  vy
                            *~ 0.5
                            *  fromIntegral h'
                            &  vec2
                            .  vx
                            *~ 0.5
                            *  fromIntegral w'
                            &  vec2
                            .  vy
                            *~ 0.5
                            *  fromIntegral h'
                            &  triToScreen
                triToScreen (Triangle v0 v1 v2) = ScreenTriangle
                    (vecToPosition v0)
                    (vecToPosition v1)
                    (vecToPosition v2)
                vecToPosition (Vec x y _z) =
                        Position (X (Prelude.round x)) (Y (Prelude.round y))
            in  triangle

    V.forM_ (mesh cubeMesh) $ \tri -> do
        let transTri = triTranslated tri
            n        = normal transTri
            doDraw   = triCulled transTri n
            lightDir = normalize light
            lum      = dotP n lightDir
            rgb =
                ( Prelude.round (lum * 255)
                , Prelude.round (lum * 255)
                , Prelude.round (lum * 255)
                )
        if doDraw
            then do
                color <- rgbColorWithRgb rgb
                drawTriangle (triDraw transTri) color
            else pure ()

    flcPopClip


drawTriangle :: ScreenTriangle -> Color -> IO ()
drawTriangle (ScreenTriangle v0 v1 v2) color = do
    flcSetColor color
    flcPolygon v0 v1 v2
    flcSetColor blackColor
    flcLoop v0 v1 v2


data Mesh = Mesh {
  mesh :: Vector Triangle
}


type Matrix = Array (Int, Int) Double

camera :: Vec
camera = Vec 0 0 0

light :: Vec
light = Vec 0 0 (-1)



multMatrixVec :: Matrix -> Vec -> Vec
multMatrixVec !mat !i =
    let !ox =
                i
                    ^. vx
                    *  mat
                    !  (0, 0)
                    +  i
                    ^. vy
                    *  mat
                    !  (1, 0)
                    +  i
                    ^. vz
                    *  mat
                    !  (2, 0)
                    +  mat
                    !  (3, 0)
        !oy =
                _vx i
                    * mat
                    ! (0, 1)
                    + _vy i
                    * mat
                    ! (1, 1)
                    + _vz i
                    * mat
                    ! (2, 1)
                    + mat
                    ! (3, 1)
        !oz =
                _vx i
                    * mat
                    ! (0, 2)
                    + _vy i
                    * mat
                    ! (1, 2)
                    + _vz i
                    * mat
                    ! (2, 2)
                    + mat
                    ! (3, 2)
        !w =
                _vx i
                    * mat
                    ! (0, 3)
                    + _vy i
                    * mat
                    ! (1, 3)
                    + _vz i
                    * mat
                    ! (2, 3)
                    + mat
                    ! (3, 3)
    in  if w /= 0.0 then Vec (ox / w) (oy / w) (oz / w) else Vec ox oy oz



cubeMesh :: Mesh
cubeMesh = Mesh $ V.fromList
    [ Triangle (Vec 0.0 0.0 0.0) (Vec 0.0 1.0 0.0) (Vec 1.0 1.0 0.0)
    , Triangle (Vec 0.0 0.0 0.0) (Vec 1.0 1.0 0.0) (Vec 1.0 0.0 0.0)

        -- EAST                                                      
    , Triangle (Vec 1.0 0.0 0.0) (Vec 1.0 1.0 0.0) (Vec 1.0 1.0 1.0)
    , Triangle (Vec 1.0 0.0 0.0) (Vec 1.0 1.0 1.0) (Vec 1.0 0.0 1.0)

        -- NORTH                                                 
    , Triangle (Vec 1.0 0.0 1.0) (Vec 1.0 1.0 1.0) (Vec 0.0 1.0 1.0)
    , Triangle (Vec 1.0 0.0 1.0) (Vec 0.0 1.0 1.0) (Vec 0.0 0.0 1.0)

        -- WEST                                                      
    , Triangle (Vec 0.0 0.0 1.0) (Vec 0.0 1.0 1.0) (Vec 0.0 1.0 0.0)
    , Triangle (Vec 0.0 0.0 1.0) (Vec 0.0 1.0 0.0) (Vec 0.0 0.0 0.0)
    , Triangle (Vec 0.0 1.0 0.0) (Vec 0.0 1.0 1.0) (Vec 1.0 1.0 1.0)
    , Triangle (Vec 0.0 1.0 0.0) (Vec 1.0 1.0 1.0) (Vec 1.0 1.0 0.0)
    , Triangle (Vec 1.0 0.0 1.0) (Vec 0.0 0.0 1.0) (Vec 0.0 0.0 0.0)
    , Triangle (Vec 1.0 0.0 1.0) (Vec 0.0 0.0 0.0) (Vec 1.0 0.0 0.0)
    ]



data SceneState = SceneState {
    scWidth :: Width
    , scHeight :: Height
    , scTheta :: Double
    }


type SceneStateRef = IORef SceneState


main :: IO ()
main = do
    let fNear  = 0.1
        fFar   = 1000.0
        fFov   = 90.0
        width  = 800
        height = 600
        fAspectRatio :: Double
        fAspectRatio = fromIntegral height / fromIntegral width
        fFovRad      = 1.0 / tan (fFov * 0.5 / 180.0 * pi)

        matProj      = array
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

    ref     <- newIORef (SceneState (Width width) (Height height) 0.0)

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

