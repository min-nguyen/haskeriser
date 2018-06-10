{-# LANGUAGE OverloadedStrings #-}

module Matrix
    where

import Prelude
import Prelude hiding (any, mapM_)
import Control.Monad hiding (mapM_)
import Data.Foldable hiding (elem)
import Data.Maybe
import Data.Word8
import Data.List
import Data.Matrix as Matrix
import Data.Cross
import Foreign.C.Types
import SDL.Vect
import SDL (($=))
import qualified SDL
import SDL_Aux
import Control.Lens
import Camera
import qualified Data.Vector as V

cam_projection_matrix :: Camera -> Matrix Double
cam_projection_matrix cam = fromList 4 4 [1, 0, 0, 0,
                                          0, 1, 0, 0,
                                          0, 0, 1, 0,
                                          0, 0, -1/z, 1]
                            where V4 x y z w = position cam

viewport_matrix :: Double -> Double -> Double -> Double -> Matrix Double
viewport_matrix x y w h = fromList 4 4 [w/2.0,   0,         0,          x+w/2.0,
                                        0,       h/2.0,     0,          y+h/2.0,
                                        0,       0,         255/2.0,    255/2.0,
                                        0,       0,         0,          1]

clamp :: Double -> Double -> Double -> Double
clamp x minval maxval = min (max x minval) maxval

replaceAt :: Double -> Int -> V.Vector Double -> V.Vector Double 
replaceAt newElement n array = V.take n array V.++  (newElement `V.cons` V.drop (n + 1) array)

toMatV2 :: V2 Double -> Matrix Double
toMatV2 (V2 a b) = fromList 2 1 [a, b]

toMatV3 :: V3 Double -> Matrix Double
toMatV3 (V3 a b c) = fromList 3 1 [a, b, c]

toMatV4 :: V4 Double -> Matrix Double
toMatV4 (V4 a b c d) = fromList 4 1 [a, b, c, d]

fromV3toMatV4 ::  V3 Double -> Matrix Double
fromV3toMatV4 (V3 a b c) = fromList 4 1 [a, b, c, 1.0]

fromMatV2 :: Matrix Double -> V2 Double
fromMatV2 m  = case toLists m of (x:xs) -> (\(x:y:_) -> V2 x y) $ head $ toLists m
                                 [] -> V2 0 0 

fromMatV3 :: Matrix Double -> V3 Double
fromMatV3 m  = case toLists m of (x:xs) -> (\(x:y:z:_) -> V3 x y z) $ head $ toLists m
                                 [] -> V3 0 0 0 

fromMatV4 :: Matrix Double -> V4 Double
fromMatV4 m  =  case toLists m of (x:xs) -> (\(x:y:z:w:_) -> V4 x y z w) $ Matrix.toList m
                                  [] -> V4 0 0 0 0

fromMatV4toV3 :: Matrix Double -> V3 Double
fromMatV4toV3 m  =  case toLists m of (x:xs) -> (\(x:y:z:w:_) -> V3 (x/w) (y/w) (z/w)) $ Matrix.toList m
                                      [] -> V3 0 0 0

fromMatV4toV3I :: Matrix Double -> V3 Int
fromMatV4toV3I m  =  case toLists m of (x:xs) -> (\(x:y:z:w:_) -> V3 (floor x) (floor y) (floor z)) $ Matrix.toList m
                                       [] -> V3 0 0 0

fromMatV3I :: Matrix Double -> V3 Int
fromMatV3I m  = case toLists m of (x:xs) -> (\(x:y:z:_) -> V3 (floor x) (floor y) (floor z)) $ head $ toLists m
                                  [] -> V3 0 0 0

fromMatV2sq :: Matrix Double -> (V2 Double, V2 Double)
fromMatV2sq m  = let [va, vb] = map (\[x, y] -> V2 x y) $ toLists m
                in (va, vb)

fromMatV3sq :: Matrix Double -> (V3 Double, V3 Double, V3 Double)
fromMatV3sq m = let [va, vb, vc] = map (\[x, y, z] -> V3 x y z) $ toLists m
                in (va, vb, vc)

fromMatV4sq :: Matrix Double -> (V4 Double, V4 Double, V4 Double, V4 Double)
fromMatV4sq m = let [va, vb, vc, vd] = map (\[x, y, z, w] -> V4 x y z w) $ toLists m
                in (va, vb, vc, vd)

or_V3 :: V3 Double -> V3 Double -> V3 Double
or_V3 (V3 ax ay az) (V3 bx by bz) = V3 (ay * bz - az * by)  (az * bx - ax * bz)  (ax * by - ay * bx)

map_V2 :: (a -> b) -> V2 a -> V2 b
map_V2 f (V2 x y ) = V2 (f x) (f y)

map_V3 :: (a -> b) -> V3 a -> V3 b
map_V3 f (V3 x y z) = V3 (f x) (f y) (f z)

norm_V3 :: V3 Double  -> V3 Double
norm_V3 (V3 ax ay az) = let magnitude = sqrt((ax * ax) + (ay * ay) + (az * az))
                        in V3 (ax/magnitude) (ay/magnitude) (az/magnitude)

-- norm_V3I :: V3 Int  -> V3 Int
-- norm_V3I (V3 ax ay az) = let magnitude = sqrt((ax * ax) + (ay * ay) + (az * az))
--                         in V3 (ax/magnitude) (ay/magnitude) (az/magnitude)

norm_V4 :: V4 Double  -> V4 Double
norm_V4 (V4 ax ay az aw) = let magnitude = sqrt((ax * ax) + (ay * ay) + (az * az) + (aw * aw))
                           in V4 (ax/magnitude) (ay/magnitude) (az/magnitude) (aw/magnitude)

stringListToV3List :: [String] -> [V3 Double]
stringListToV3List str = case str of (x:xs) -> let (a:b:c:_) = take 3 (str) in ((V3 (read a) (read b) (read c)):(stringListToV3List $ drop 3 str))
                                     [] -> []

stringListToV2List :: [[String]] -> [V2 Double]
stringListToV2List str = [(V2 (read a) (read b) ) | (a:b:_) <- (str) ]

stringListToV3ListI :: [[String]] -> [V3 Integer]
stringListToV3ListI str = [(V3 (read a) (read b) (read c) ) | (a:b:c:_) <- (str) ]

mapTuple2 :: (a -> b) -> (a, a) -> (b, b)
mapTuple2 f (a1, a2) = (f a1, f a2)

mapTuple3 :: (a -> b) -> (a, a, a) -> (b, b, b)
mapTuple3 f (a1, a2, a3) = (f a1, f a2, f a3)
