import Codec.Picture
type Vector3 = ( Double, Double, Double )
type Origin = Vector3
type Normal = Vector3
type Direction = Vector3
type Location = Vector3
type Radius = Double
type FocalLength = Double
type Color = (Int,Int,Int) 
data Surface = Sphere Origin Radius Color
             | Plane Origin Normal Color
data Line = Line Origin Direction
type Scene = [ Surface ]
type Front = Direction
type Up = Direction
type Rays = [Direction]
data Camera = Camera Location Rays
data World = World Camera [Surface]
data Light = DirectionalLight Vector3

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
-- fl focallength distance between film and pinhole
cameraRays:: Int -> Int -> Double -> Double -> [Vector3]
cameraRays xc yc ar fl = [ ( x, y, (-fl) ) | y <- _y_cam_coords yc (1/ar), x <- _x_cam_coords xc ar ] 
defaultCamera focallength = Camera ( 0, 0, 0 ) (cameraRays 640 480 1.33 focallength) 
light = DirectionalLight (0.6, 0.6, 1)

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
sphereIntersectionFormula support direction (Sphere location radius _) = ( a, b, c )
    where a = (absolute direction) ** 2
          b = 2 * ( direction *. ( support -. location ))
          c = ((absolute ( support -. location )) ** 2) - ( radius ** 2 ) 

calcD:: Vector3 -> Double
calcD ( a, b , c) = (b ** 2) - ( 4 * a * c )

intersection:: Line -> Surface -> [Double] 
intersection (Line support direction) sphere 
    | d < 0 = [] 
    | d == 0 = [ (-b) / (2*a) ]
    | d > 0 = [ ((-b) + (d ** (1/2))) / (2*a), ((-b) - (d ** (1/2))) / (2*a) ]
    where (a, b, c) = sphereIntersectionFormula support direction sphere
          d = calcD (a, b, c)

sm:: Double -> Vector3 -> Vector3
sm scalar (v1, v2, v3) = ( scalar*v1, scalar*v2, scalar*v3)

distance2Coord:: Line -> Double -> Vector3
distance2Coord (Line origin direction) distance = origin +. ( distance `sm` direction )

surfaceNormal:: Surface -> Location -> Vector3
surfaceNormal (Sphere origin _ _) location = normalize ( location -. origin )

surface = Sphere ( 0, 0, -10.0) 5 (255, 255, 0)
plane = Plane (0, 0, 0 ) ( 0, 1, 0 )
--scene = [ sphere, plane ]
camera = defaultCamera 1 
Camera _ rays = camera
getPix (a:as) _ _ = (as, a)

shade :: Light -> Surface -> Camera -> Vector3 -> PixelRGBA8
shade (DirectionalLight lv) (Sphere origin radius (r, g, b)) (Camera location _) ray 
    | intersections == [] = PixelRGBA8 0 0 30 255
    | factor < 0 = PixelRGBA8 0 0 0 255
    | otherwise = (PixelRGBA8 sr sg sb 255)
    where snv = surfaceNormal (Sphere origin radius (r, g, b)) (distance2Coord (Line location ray) (minimum intersections))
          factor = ((normalize lv) *. snv)
          sr = round(fromIntegral(r)*factor)
          sg = round(fromIntegral(g)*factor)
          sb = round(fromIntegral(b)*factor)
          intersections = intersection (Line location ray) (Sphere origin radius (r,g,b))

-- img = map (\x -> if (_, x ) == [] then (PixelRGBA8 0 0 0 255 ) else ( (surfaceNormal surface) . (distance2Coord ray) . minimum) $ map ($ surface) $ map (\x r -> (r, intersection (0, 0, 0) r surface)) rays
img = map (shade light surface camera) rays 

(_, expimg) = generateFoldImage (getPix) img 640 480

main :: IO()
main = do
    writePng "here.png" expimg
