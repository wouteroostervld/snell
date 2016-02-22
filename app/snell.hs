import Codec.Picture
import GHC.Exts
import Data.Maybe
type Vector3 = ( Double, Double, Double )
type Origin = Vector3
type Normal = Vector3
type Direction = Vector3
type Location = Vector3
type Radius = Double
type FocalLength = Double
type Color = (Int,Int,Int)
data Surface = Sphere Origin Radius ( Vector3 -> Color ) ( Light -> [Surface] -> Surface -> Vector3 -> Vector3 -> Vector3 -> Vector3 )
             | Plane Origin Normal  ( Vector3 -> Color ) ( Light -> [Surface] -> Surface -> Vector3 -> Vector3 -> Vector3 -> Vector3 )
data Line = Line Origin Direction
type Scene = [ Surface ]
type Front = Direction
type Up = Direction
type Rays = [Direction]
data Camera = Camera Location Rays
data World = World Camera [Surface]
data Light = DirectionalLight Vector3 | PointLight Vector3 Double

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
cameraRays xc yc ar fl = [ ( x, y, (-fl) ) | y <- _y_cam_coords yc (1/ar), x <- _x_cam_coords xc ar ]
defaultCamera focallength = Camera ( 0, 0, 0 ) (map normalize $ cameraRays 1280 720 1.78 focallength)

absolute:: Vector3 -> Double
absolute ( x, y, z ) = ( a + b + c ) ** ( 1/2 )
        where a = x ** 2
              b = y ** 2
              c = z ** 2

normalize:: Vector3 -> Vector3
normalize ( x , y , z ) = ( x/l, y/l, z/l )
        where l = absolute ( x, y, z )

-- inproduct between vectors
-- cosine of angle when used with normalized vectors
-- AKA magnitude*|a||b| of projection of the smaller on the bigger vector
(*.):: Vector3 -> Vector3 -> Double
(*.) (a1, a2, a3) (b1, b2, b3) = (a1*b1)+(a2*b2)+(a3*b3)

(-.):: Vector3 -> Vector3 -> Vector3
(-.) (a1, a2, a3) (b1, b2, b3) = ( a1 - b1, a2 - b2, a3 - b3 )

(+.):: Vector3 -> Vector3 -> Vector3
(+.) (a1, a2, a3) (b1, b2, b3) = ( a1 + b1, a2 + b2, a3 + b3 )

sphereIntersectionFormula:: Vector3 -> Vector3 -> Surface -> Vector3
sphereIntersectionFormula support direction (Sphere location radius _ _) = ( a, b, c )
    where a = 1
          b = 2 * ( direction *. ( support -. location ))
          c = ((absolute ( support -. location )) ** 2) - ( radius ** 2 )

calcD:: Vector3 -> Double
calcD ( a, b , c) = (b ** 2) - ( 4 * a * c )

intersection:: Line -> Surface -> Maybe Double
intersection (Line support direction) (Sphere origin radius color shader)
    | d < 0 = Nothing
    | x1 < 0 && x2 < 0 = Nothing
    | otherwise = Just $ min x1 x2
    where (a, b, c) = sphereIntersectionFormula support direction (Sphere origin radius color shader)
          d = calcD (a, b, c)
          x1 = ((-b) + (d ** (1/2))) / (2*a)
          x2 = ((-b) - (d ** (1/2))) / (2*a)

intersection (Line support direction) (Plane origin normal color shader)
    | (incline /= 0) && (d > 0) = Just d
    | otherwise = Nothing
    where incline = direction *. normal
          d = ( ( origin -. support ) *. normal ) / incline


sm:: Double -> Vector3 -> Vector3
sm scalar (v1, v2, v3) = ( scalar*v1, scalar*v2, scalar*v3)

distance2Coord:: Line -> Double -> Vector3
distance2Coord (Line origin direction) distance = origin +. ( distance `sm` direction )

surfaceNormal:: Surface -> Location -> Vector3
surfaceNormal (Sphere origin _ _ _) location = normalize ( location -. origin )
surfaceNormal (Plane _ normal _ _) location = normalize normal

getPix (a:as) _ _ = (as, a)

cap:: Double -> Double
cap i
    | i > 1 = 1
    | otherwise = i

flatQuantize:: Double -> Vector3 -> PixelRGBA8
flatQuantize base ( a, b, c ) = PixelRGBA8 (flat a) (flat b) (flat c) 255
    where flat x = round(255*(x/base))

expQuantize:: Double -> Double -> Vector3 -> PixelRGBA8
expQuantize exposure base ( a, b, c ) = PixelRGBA8 (expose a) (expose b) (expose c) 255
    where expose x = round (( 1 - exp (-(x/base) * exposure )) * 255)

getBase:: [Vector3] -> Double
getBase pixels = base
    where ( rs, gs, bs ) = unzip3 pixels
          base = max (maximum rs) $ max (maximum gs) (maximum bs)

shade :: Light -> [Surface] -> Camera -> Vector3 -> Vector3
shade l surfaces (Camera location _) ray
    | null intersections = ( 0, 0, 10 )
    | otherwise = case s of (Sphere _ _ _ shader) -> shader l surfaces s coord location ray
                            (Plane  _ _ _ shader) -> shader l surfaces s coord location ray
    where intersections = sortWith (\(_,d) -> fromJust d ) $ filter (\(_,d) -> d /= Nothing ) $ map (\s -> (s, intersection (Line location ray) s)) surfaces
          (s, Just d) = head intersections
          coord = distance2Coord (Line location ray) d

diffuseShader :: Light -> [Surface] -> Surface -> Vector3 -> Vector3 -> Vector3 -> Vector3
diffuseShader l surfaces s coord location ray
    | factor < 0 = ( 0, 0, 0 )
    | shadow = ( 0, 0, 0 )
    | otherwise = ( sr, sg, sb )
    where lv = case l of (DirectionalLight lv) -> lv
                         (PointLight location _) -> location -. coord
          snv = surfaceNormal s coord
          factor = case l of (DirectionalLight _) -> ((normalize lv) *. snv)
                             (PointLight _ i) -> i * ((normalize lv) *. snv) / ( 4 * pi * ( absolute lv ) ^ 2 )
          sr = fromIntegral(r)*factor
          sg = fromIntegral(g)*factor
          sb = fromIntegral(b)*factor
          ( r, g, b ) = case s of (Sphere _ _ c _) -> c coord
                                  (Plane _ _ c _ ) -> c coord
          shadow = case l of (PointLight _ _) -> not $ null $ filter (\x -> x < absolute lv) $ catMaybes $ map (intersection (Line coord_bias (normalize lv))) surfaces
                             (DirectionalLight _) -> not $ null $  catMaybes $ map (intersection (Line coord_bias (normalize lv))) surfaces
          coord_bias = (1e-7 `sm` snv ) +. coord

img = map (shade light scene camera) rays
base = getBase img
concrete_img = map (expQuantize 6 base) img
--concrete_img = map (flatQuantize base) img
(_, expimg) = generateFoldImage (getPix) concrete_img 1280 720

checker:: Color -> Color -> Double -> Vector3 -> Color
checker black white size ( x, _, z )
    | ( floor(x/size) + floor(z/size) ) `mod` 2 == 0 = black
    | otherwise                                        = white

-- scene, light and camera
sphere = Sphere ( 15, 0, -60) 15 (\_ -> (65, 65, 0)) diffuseShader
sphere2 = Sphere ( -15, 0, -45) 15 (\_ -> (65, 0, 0)) diffuseShader
plane = Plane (0, -15, 0 ) ( 0, 1, 0 ) (checker (32, 32, 32) (127, 127, 127) 10) diffuseShader
scene = [ sphere, plane, sphere2 ]
camera = defaultCamera 1
light = PointLight (30, 30, 0) 0.2e5
-- light = DirectionalLight ( 1, 1, 1)
Camera _ rays = camera

main :: IO()
main = do
    writePng "here.png" expimg
