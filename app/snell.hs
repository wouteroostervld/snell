import Codec.Picture
import GHC.Exts
import Data.Maybe
import Debug.Trace
import Linear
type Vector3 = V3 Double
type Origin = Vector3
type Normal = Vector3
type Direction = Vector3
type Location = Vector3
type Radius = Double
type FocalLength = Double
type Color = V3 Int
data Surface = Sphere Origin Radius ( Vector3 -> Color ) ( Light -> [Surface] -> Surface -> Vector3 -> Vector3 -> Vector3)
             | Plane Origin Normal  ( Vector3 -> Color ) ( Light -> [Surface] -> Surface -> Vector3 -> Vector3 -> Vector3)
data Line = Line Origin Direction
type Scene = [ Surface ]
type Front = Direction
type Up = Direction
type Rays = [Direction]
data Camera = Camera Location Rays
data World = World Camera [Surface]
data Light = DirectionalLight Vector3 Double | PointLight Vector3 Double

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

absolute = norm

-- inproduct between vectors
-- cosine of angle when used with normalized vectors
-- AKA magnitude*|a||b| of projection of the smaller on the bigger vector
(*.) = dot

(-.) = (-)

(+.) = (+)

sphereIntersectionFormula:: Vector3 -> Vector3 -> Surface -> Vector3
sphereIntersectionFormula support direction (Sphere location radius _ _) = (V3 a b c)
    where a = 1
          b = 2 * ( direction *. ( support -. location ))
          c = ((absolute ( support -. location )) ** 2) - ( radius ** 2 )

calcD:: Vector3 -> Double
calcD (V3 a b c) = (b ** 2) - ( 4 * a * c )

intersection:: Line -> Surface -> Maybe Double
intersection (Line support direction) (Sphere origin radius color shader)
    | d < 0 = Nothing
    | x1 < 0 && x2 < 0 = Nothing
    | otherwise = Just $ min x1 x2
    where (V3 a b c) = sphereIntersectionFormula support direction (Sphere origin radius color shader)
          d = calcD (V3 a b c)
          x1 = ((-b) + (d ** (1/2))) / (2*a)
          x2 = ((-b) - (d ** (1/2))) / (2*a)

intersection (Line support direction) (Plane origin normal color shader)
    | (incline /= 0) && (d > 0) = Just d
    | otherwise = Nothing
    where incline = direction *. normal
          d = ( ( origin -. support ) *. normal ) / incline


sm:: Double -> Vector3 -> Vector3
sm = (*^)

distance2Coord:: Line -> Double -> Vector3
distance2Coord (Line origin direction) distance = origin +. ( distance `sm` direction )

surfaceNormal:: Surface -> Location -> Vector3
surfaceNormal (Sphere origin _ _ _) hitpoint = normalize ( hitpoint -. origin )
surfaceNormal (Plane _ normal _ _) _ = normalize normal

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

castRay :: Light -> [Surface] -> Camera -> Vector3 -> Vector3
castRay l surfaces (Camera camOrigin _) ray
    | null intersections = (V3 0 0 10)
    | otherwise = case s of (Sphere _ _ _ shader) -> shader l surfaces s hitpoint ray
                            (Plane  _ _ _ shader) -> shader l surfaces s hitpoint ray
    where intersections = sortWith (\(_,d) -> fromJust d ) $ filter (\(_,d) -> d /= Nothing ) $ map (\s -> (s, intersection (Line camOrigin ray) s)) surfaces
          (s, Just d) = head intersections
          hitpoint = distance2Coord (Line camOrigin ray) d

diffuseShader :: Double -> Light -> [Surface] -> Surface -> Vector3 -> Vector3 -> Vector3
diffuseShader albedo l surfaces s hitpoint ray
    | factor < 0 = (V3 0 0 0)
    | shadow = (V3 0 0 0)
    | otherwise = (V3 sr sg sb)
    where lv = case l of (DirectionalLight lv _) -> lv
                         (PointLight location _) -> location -. hitpoint
          snv = surfaceNormal s hitpoint
          factor = case l of (DirectionalLight _ i) -> (albedo / pi ) * i * ((normalize lv) *. snv)
                             (PointLight _ i) -> (albedo / pi) * i * ((normalize lv) *. snv) / ( 4 * pi * ( absolute lv ) ^ 2 )
          sr = fromIntegral(r)*factor
          sg = fromIntegral(g)*factor
          sb = fromIntegral(b)*factor
          (V3 r g b) = case s of (Sphere _ _ c _) -> c hitpoint
                                 (Plane _ _ c _ ) -> c hitpoint
          shadow = case l of (PointLight _ _) -> not $ null $ filter (\x -> x < absolute lv) $ catMaybes $ map (intersection (Line hitpoint_bias (normalize lv))) surfaces
                             (DirectionalLight _ _) -> not $ null $ catMaybes $ map (intersection (Line hitpoint_bias (normalize lv))) surfaces
          hitpoint_bias = (1e-7 `sm` snv ) +. hitpoint

defaultDiffuseShader = (diffuseShader pi)

nullShader :: Light -> [Surface] -> Surface -> Vector3 -> Vector3 -> Vector3
nullShader _ _ s hitpoint _
    = (V3 (fromIntegral(r)) (fromIntegral(g)) (fromIntegral(b)))
    where (V3 r g b) = case s of (Sphere _ _ c _) -> c hitpoint
                                 (Plane _ _ c _ ) -> c hitpoint

reflectionShader :: Light -> [Surface] -> Surface -> Vector3 -> Vector3 -> Vector3
reflectionShader l surfaces s hitpoint ray
    = (castRay l surfaces (Camera hitpoint_bias []) rv)
    where rv = ( ray -. ( ( 2 * ( ray *. snv ) ) `sm` snv ) )
          snv = surfaceNormal s hitpoint
          hitpoint_bias = (1e-7 `sm` snv ) +. hitpoint
          (V3 r g b) = case s of (Sphere _ _ c _) -> c hitpoint
                                 (Plane _ _ c _ ) -> c hitpoint

schlickShader :: Double -> Light -> [Surface] -> Surface -> Vector3 -> Vector3 -> Vector3
schlickShader r0 l surfaces s hitpoint ray
    -- r0 is the reflectance at a view and reflectance angle of 0 degrees.
    -- = trace ((show viewangle) ++ ": " ++ (show r) ++ " r0: " ++ (show r0) ++ " n2: " ++ (show n2)) (reflection +. diffusion)
    = reflection +. diffusion
    where snv = surfaceNormal s hitpoint
          viewangle = abs $ ray *. snv
          r = r0 + ((1 - r0) * (( 1 - viewangle) ** 5))
          n1 = 1
          reflection = r `sm` (reflectionShader l surfaces s hitpoint ray)
          diffusion = (1 - r) `sm` (defaultDiffuseShader l surfaces s hitpoint ray)

schlickMetalShader:: Double -> Double -> Light -> [Surface] -> Surface -> Vector3 -> Vector3 -> Vector3
    -- from "Fresnel Term Approximations for Metals" Lazányi and Szirmay-Kalos
schlickMetalShader n k l surfaces s hitpoint ray
    -- = trace ( "viewangle:" ++  (show viewangle)  ++ " r:" ++ (show r) ) reflection +. diffusion
    = reflection +. diffusion
    where snv = surfaceNormal s hitpoint
          viewangle = abs $ ray *. snv
          r = ((( n - 1) ** 2) + ( 4 * n * (( 1 - viewangle ) ** 5)) + (k ** 2)) / (((n + 1) ** 2) + (k ** 2))
          reflection = r `sm` taint (getColor s hitpoint) (reflectionShader l surfaces s hitpoint ray)
          diffusion = (1 - r) `sm` (defaultDiffuseShader l surfaces s hitpoint ray)

clamp:: Double -> Double
clamp x = min 1 (max 0 x)

getColor s hitpoint = case s of (Sphere _ _ c _) -> c hitpoint
                                (Plane _ _ c _ ) -> c hitpoint

taint:: Color -> Vector3 -> Vector3
taint color shade = (V3 (component c1 s2) (component c2 s2) (component c3 s3))
    where (V3 c1 c2 c3) = color
          (V3 s1 s2 s3) = shade
          component c s = s*fromIntegral(c)/255

reflectanceFromRefractionIndex n1 = r0
          where r0 = ((n1-n2)/(n1+n2)) ** 2
                n2 = 1 -- vaccuum (approx air)

img = map (castRay light scene camera) rays
base = getBase img
concrete_img = map (expQuantize 8 base) img
--concrete_img = map (flatQuantize base) img
(_, expimg) = generateFoldImage (getPix) concrete_img 1280 720

checker:: Color -> Color -> Double -> Vector3 -> Color
checker black white size (V3 x _ z)
    | ( floor(x/size) + floor(z/size) ) `mod` 2 == 0 = black
    | otherwise                                      = white

-- scene, light and camera
sphere = Sphere (V3  15 0 (-60)) 15 (\_ -> (V3 255 215 0)) (schlickMetalShader 0.229 6.79)
sphere2 = Sphere (V3 (-15) 0 (-45)) 15 (\_ -> (V3 255 215 1)) (schlickShader $ reflectanceFromRefractionIndex 1.54)
plane = Plane (V3 0 (-15) 0 ) (V3 0 1 0 ) (checker (V3 32 32 32) (V3 127 127 127) 10) defaultDiffuseShader
scene = [ sphere, plane, sphere2 ]
camera = defaultCamera 1
light = PointLight (V3 30 30 0) 0.1e5
--light = DirectionalLight (V3  1 1 1) 0.2e5
Camera _ rays = camera

main :: IO()
main = do
    writePng "here.png" expimg
