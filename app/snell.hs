{-# LANGUAGE RecordWildCards #-}
import Codec.Picture
import GHC.Exts
import Data.Maybe
import Debug.Trace
import Linear
import System.Environment
import System.Exit
import Control.Monad

type Vector3 = V3 Double
type Vertex = Vector3
type Origin = Vector3
type Position = Vector3
type Normal = Vector3
type Direction = Vector3
type Radius = Double
type FocalLength = Double
type ColorF = Vector3
type Color = V3 Int
type Ray = Vector3
type Hitpoint = Vector3


data Triangle = Triangle { triangleA :: Vertex
                         , triangleB :: Vertex
                         , triangleC :: Vertex
                         }
data Surface = Surface { surfaceIntersection :: ( Line -> Maybe Double )
                       , surfaceTexture :: ( Vector3 -> Color )
                       , surfaceShader :: ( World -> Hitpoint -> Normal -> Color -> Ray -> ColorF)
                       , surfaceNormal :: ( Hitpoint -> Normal )
                       }
data Sphere = Sphere { spherePosition :: Position
                     , sphereRadius :: Radius
                     }
data Plane = Plane { planePosition :: Position
                   , planeNormal :: Normal
                   }
data Mesh = Mesh { meshPosition :: Position
                 , meshTriangles :: [Triangle]
                 }

data Line = Line { lineSupport :: Position
                 , lineDirection :: Direction
                 }
type Scene = [ Surface ]
type Front = Direction
type Up = Direction
type Rays = [Direction]
data Camera = Camera Position Rays
data World = World { worldCamera :: Camera
                   , worldLight :: Light
                   , worldSurfaces :: [Surface] }
data Light = DirectionalLight { dlDirection :: Direction
                              , dlIntensity :: Double
                              }
           | PointLight { plPosition :: Position
                        , plIntensity :: Double
                        }

-- _x_cam_coords count aspectratio (from -1 to 1 or -ar to ar if ar < 1)
_x_cam_coords:: Int -> Double -> [Double]
_x_cam_coords c ar = map (+start) $ map ((end-start)*) $ map (/fromIntegral(c)) $ map (fromIntegral) [1..c]
            where start = if ar > 1 then -1 else (-(1/ar))
                  end = if ar > 1 then 1 else (1/ar)

-- _y_cam_coords count aspectratio (from 1 to -1 or ar to -ar if 1/ar < 1)
_y_cam_coords:: Int -> Double -> [Double]
_y_cam_coords c ar = _x_cam_coords c (-(1/ar))

-- xc pixelcount x
-- xc pixelcount y
-- ar aspectratio x-size/y-size
-- fl 10focallength distance between film and pinhole
cameraRays:: Int -> Int -> Double -> Double -> [Vector3]
cameraRays xc yc ar fl = [ (V3 x y (-fl)) | y <- _y_cam_coords yc (1/ar), x <- _x_cam_coords xc ar ]
defaultCamera focallength = Camera (V3 0 0 0) (map normalize $ cameraRays 1280 720 1.78 focallength)

-- inproduct between vectors
-- cosine of angle when used with normalized vectors
-- AKA magnitude*|a||b| of projection of the smaller on the bigger vector
(*.) = dot
(-.) = (-)
(+.) = (+)

sphereIntersectionFormula:: Vector3 -> Vector3 -> Sphere -> Vector3
sphereIntersectionFormula support direction Sphere{..} = (V3 a b c)
    where a = 1
          b = 2 * ( direction *. ( support -. spherePosition ))
          c = ((norm ( support -. spherePosition )) ** 2) - ( sphereRadius ** 2 )

calcD:: Vector3 -> Double
calcD (V3 a b c) = (b ** 2) - ( 4 * a * c )

sphereIntersection:: Sphere -> Line -> Maybe Double
sphereIntersection sphere@Sphere{..} Line{..}
    | d < 0 = Nothing
    | x1 < 0 && x2 < 0 = Nothing
    | otherwise = Just $ min x1 x2
    where (V3 a b c) = sphereIntersectionFormula lineSupport lineDirection sphere
          d = calcD (V3 a b c)
          x1 = ((-b) + (d ** (1/2))) / (2*a)
          x2 = ((-b) - (d ** (1/2))) / (2*a)

planeIntersection:: Plane -> Line -> Maybe Double
planeIntersection Plane{..} Line{..}
    | (incline /= 0) && (d > 0) = Just d
    | otherwise = Nothing
    where incline = lineDirection *. planeNormal
          d = ( ( planePosition -. lineSupport ) *. planeNormal ) / incline

distance2Coord:: Line -> Double -> Vector3
distance2Coord Line{..} distance = lineSupport +. ( distance *^ lineDirection )

sphereSurfaceNormal Sphere{..} hitpoint = normalize ( hitpoint -. spherePosition )

getPix (a:as) _ _ = (as, a)

cap:: Double -> Double
cap i
    | i > 1 = 1
    | otherwise = i

flatQuantize:: Double -> Vector3 -> PixelRGBA8
flatQuantize base (V3 a b c) = PixelRGBA8 (flat a) (flat b) (flat c) 255
    where flat x = round(255*(x/base))

expQuantize:: Double -> Double -> Vector3 -> PixelRGBA8
expQuantize exposure base (V3 a b c) = PixelRGBA8 (expose a) (expose b) (expose c) 255
    where expose x = round (( 1 - exp (-(x/base) * exposure )) * 255)

getBase:: [Vector3] -> Double
getBase pixels = base
    where ( rs, gs, bs ) = unzip3 $ map (\(V3 a b c) -> (a, b, c)) pixels
          base = max (maximum rs) $ max (maximum gs) (maximum bs)

castRay :: World -> Vector3 -> Vector3
castRay w@World{..} ray
    | null intersections = (V3 0 0 10)
    | otherwise = surfaceShader s w hitpoint snv color ray
    where intersections = sortWith (\(_,d) -> fromJust d ) $ filter (\(_,d) -> d /= Nothing ) $ map (\s -> (s, surfaceIntersection s (Line camOrigin ray))) worldSurfaces
          (s, Just d) = head intersections
          hitpoint = distance2Coord (Line camOrigin ray) d
          snv = surfaceNormal s hitpoint
          (Camera camOrigin _) = worldCamera
          color = surfaceTexture s hitpoint

diffuseShader :: Double -> World -> Hitpoint -> Normal -> Color -> Ray -> ColorF
diffuseShader albedo World{..} hitpoint snv color ray
    | factor < 0 = (V3 0 0 0)
    | shadow = (V3 0 0 0)
    | otherwise = (V3 sr sg sb)
    where lv = case worldLight of DirectionalLight{..} -> dlDirection
                                  PointLight{..} -> plPosition -. hitpoint
          factor = case worldLight of (DirectionalLight _ i) -> (albedo / pi ) * i * ((normalize lv) *. snv)
                                      (PointLight _ i) -> (albedo / pi) * i * ((normalize lv) *. snv) / ( 4 * pi * ( norm lv ) ^ 2 )
          sr = fromIntegral(r)*factor
          sg = fromIntegral(g)*factor
          sb = fromIntegral(b)*factor
          (V3 r g b) = color
          shadow = case worldLight of PointLight{..} -> not $ null $ filter (\x -> x < norm lv) $ catMaybes $ map (\s -> surfaceIntersection s (Line hitpoint_bias (normalize lv))) worldSurfaces
                                      DirectionalLight{..} -> not $ null $ catMaybes $ map (\s -> surfaceIntersection s (Line hitpoint_bias (normalize lv))) worldSurfaces
          hitpoint_bias = (1e-7 *^ snv ) +. hitpoint

defaultDiffuseShader = (diffuseShader pi)

nullShader :: World -> Hitpoint -> Normal -> Color -> Ray -> ColorF
nullShader _ hitpoint _ color _
    = (V3 (fromIntegral(r)) (fromIntegral(g)) (fromIntegral(b)))
    where (V3 r g b) = color



perfectReflectionShader = reflectionShader (V3 1.0 1.0 1.0)

reflectionShader :: ColorF -> World -> Hitpoint -> Normal -> Color -> Ray -> ColorF
reflectionShader reflectance w@World{..} hitpoint snv _ ray
    = reflectance * (castRay (World (Camera hitpoint_bias []) worldLight worldSurfaces) rv)
    where rv = ( ray -. ( ( 2 * ( ray *. snv ) ) *^ snv ) )
          hitpoint_bias = (1e-7 *^ snv ) +. hitpoint

schlickShader :: Double -> World -> Hitpoint -> Normal -> Color -> Ray -> ColorF
schlickShader r0 w@World{..} hitpoint snv color ray
    = reflection +. diffusion
    where viewangle = abs $ ray *. snv
          r = r0 + ((1 - r0) * (( 1 - viewangle) ** 5))
          n1 = 1
          reflection = r *^ (perfectReflectionShader w hitpoint snv color ray)
          diffusion = (1 - r) *^ (defaultDiffuseShader w hitpoint snv color ray)

schlickMetalShader:: Double -> Double -> World -> Hitpoint -> Normal -> Color -> Ray -> ColorF
    -- from "Fresnel Term Approximations for Metals" Lazányi and Szirmay-Kalos
schlickMetalShader n k w@World{..} hitpoint snv color ray
    -- = trace ( "viewangle:" ++  (show viewangle)  ++ " r:" ++ (show r) ) reflection +. diffusion
    = reflection +. diffusion
    where viewangle = abs $ ray *. snv
          r = ((( n - 1) ** 2) + ( 4 * n * (( 1 - viewangle ) ** 5)) + (k ** 2)) / (((n + 1) ** 2) + (k ** 2))
          reflection = r *^ taint color (perfectReflectionShader w hitpoint snv color ray)
          diffusion = (1 - r) *^ (defaultDiffuseShader w hitpoint snv color ray)

clamp:: Double -> Double
clamp x = min 1 (max 0 x)

taint:: Color -> Vector3 -> Vector3
taint color shade = (V3 (component c1 s2) (component c2 s2) (component c3 s3))
    where (V3 c1 c2 c3) = color
          (V3 s1 s2 s3) = shade
          component c s = s*fromIntegral(c)/255

reflectanceFromRefractionIndex n1 = r0
          where r0 = ((n1-n2)/(n1+n2)) ** 2
                n2 = 1 -- vaccuum (approx air)

world = World camera light scene
img = map (castRay world) rays
base = getBase img
concrete_img = map (expQuantize 8 base) img
--concrete_img = map (flatQuantize base) img
(_, expimg) = generateFoldImage (getPix) concrete_img 1280 720

checker:: Color -> Color -> Double -> Vector3 -> Color
checker black white size (V3 x _ z)
    | ( floor(x/size) + floor(z/size) ) `mod` 2 == 0 = black
    | otherwise                                      = white

plainColor:: Color -> Vector3 -> Color
plainColor x _ = x

-- scene, light and camera
sphereSurface :: Vector3 -> Double -> ( Hitpoint -> Color ) -> ( World -> Hitpoint -> Normal -> Color -> Ray -> ColorF) -> Surface
sphereSurface position radius texture shader
  = Surface (sphereIntersection sphere)
            (texture)
            (shader)
            (sphereSurfaceNormal sphere)
  where sphere = Sphere position radius
sphere1 = sphereSurface (V3 (15) 10 (-90)) 60 (plainColor (V3 255 215 1)) (reflectionShader (V3 0 1 0))
sphere2 = sphereSurface (V3 (-15) 0 (-45)) 15 (plainColor (V3 255 215 1)) (schlickShader $ reflectanceFromRefractionIndex 1.54)
sphere3 = sphereSurface (V3 (-15) 0 (-150)) 15 (plainColor (V3 0 255 255)) (schlickShader $ reflectanceFromRefractionIndex 1.54)
sphere4 = sphereSurface (V3 (5) 0 (30)) 15 (plainColor (V3 255 0 255)) (schlickShader $ reflectanceFromRefractionIndex 1.54)

planeSurface position normal texture shader
  = Surface (planeIntersection plane)
            (texture)
            (shader)
            (\_ -> normal)
  where plane = Plane position normal

plane = planeSurface (V3 0 (-15) 0 ) (normalize (V3 0 1 0.3)) (\x -> checker (V3 32 32 32) (V3 127 127 127) 10 (rotate (axisAngle (V3 0 1 0) (1.75 * pi)) x)) defaultDiffuseShader
scene = [ plane, sphere1, sphere2, sphere3, sphere4 ]
camera = defaultCamera 0.8
light = PointLight (V3 30 30 0) 0.1e4
--light = DirectionalLight (V3  1 1 1) 0.2e5
Camera _ rays = camera

main :: IO()
main = do
  args <- getArgs
  when (length args > 1) $ die "Too many args."
  let fileName = case args of [] -> "here.png"
                              (fileName:[]) -> fileName
  writePng fileName expimg
