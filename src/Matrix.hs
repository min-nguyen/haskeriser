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
import SDLx
import Control.Lens
import Camera
import qualified Data.Vector as V
import Data.Vec as Vec
import Util

-- toMatVec2 :: Vec2 Double -> Matrix Double
-- toMatVec2 (Vec2 a b) = fromList 2 1 [a, b]

-- toMatVec3 :: Vec3 Double -> Matrix Double
-- toMatVec3 (Vec3 a b c) = fromList 3 1 [a, b, c]

-- toMatVec4 :: Vec4 Double -> Matrix Double
-- toMatVec4 (Vec4 a b c d) = fromList 4 1 [a, b, c, d]

-- fromVec3toMatVec4 ::  Vec3 Double -> Matrix Double
-- fromVec3toMatVec4 (Vec3 a b c) = fromList 4 1 [a, b, c, 1.0]

-- fromMatVec2 :: Matrix Double -> Vec2 Double
-- fromMatVec2 m  = case toLists m of (x:xs) -> (\(x:y:_) -> Vec2 x y) $ head $ toLists m
--                                  [] -> Vec2 0 0 

-- fromMatVec3 :: Matrix Double -> Vec3 Double
-- fromMatVec3 m  = case toLists m of (x:xs) -> (\(x:y:z:_) -> Vec3 x y z) $ head $ toLists m
--                                  [] -> Vec3 0 0 0 

-- fromMatVec4 :: Matrix Double -> Vec4 Double
-- fromMatVec4 m  =  case toLists m of (x:xs) -> (\(x:y:z:w:_) -> Vec4 x y z w) $ Matrix.toList m
--                                   [] -> Vec4 0 0 0 0

-- fromMatVec4toVec3 :: Matrix Double -> Vec3 Double
-- fromMatVec4toVec3 m  =  case toLists m of (x:xs) -> (\(x:y:z:w:_) -> Vec3 (x/w) (y/w) (z/w)) $ Matrix.toList m
--                                       [] -> Vec3 0 0 0

-- fromMatVec4toVec3I :: Matrix Double -> Vec3 Int
-- fromMatVec4toVec3I m  =  case toLists m of (x:xs) -> (\(x:y:z:w:_) -> Vec3 (floor x) (floor y) (floor z)) $ Matrix.toList m
--                                        [] -> Vec3 0 0 0

-- fromMatVec3I :: Matrix Double -> Vec3 Int
-- fromMatVec3I m  = case toLists m of (x:xs) -> (\(x:y:z:_) -> Vec3 (floor x) (floor y) (floor z)) $ head $ toLists m
--                                   [] -> Vec3 0 0 0

-- fromMatVec2sq :: Matrix Double -> (Vec2 Double, Vec2 Double)
-- fromMatVec2sq m  = let [va, vb] = map (\[x, y] -> Vec2 x y) $ toLists m
--                 in (va, vb)

-- fromMatVec3sq :: Matrix Double -> (Vec3 Double, Vec3 Double, Vec3 Double)
-- fromMatVec3sq m = let [va, vb, vc] = map (\[x, y, z] -> Vec3 x y z) $ toLists m
--                 in (va, vb, vc)

-- fromMatVec4sq :: Matrix Double -> (Vec4 Double, Vec4 Double, Vec4 Double, Vec4 Double)
-- fromMatVec4sq m = let [va, vb, vc, vd] = map (\[x, y, z, w] -> Vec4 x y z w) $ toLists m
--                 in (va, vb, vc, vd)

-- map_Vec2 :: (a -> b) -> Vec2 a -> Vec2 b
-- map_Vec2 f (Vec2 x y ) = Vec2 (f x) (f y)

-- map_Vec3 :: (a -> b) -> Vec3 a -> Vec3 b
-- map_Vec3 f (Vec3 x y z) = Vec3 (f x) (f y) (f z)

-- norm_Vec3 :: Vec3 Double  -> Vec3 Double
-- norm_Vec3 (Vec3 ax ay az) = let magnitude = sqrt((ax * ax) + (ay * ay) + (az * az))
--                         in Vec3 (ax/magnitude) (ay/magnitude) (az/magnitude)

-- norm_Vec4 :: Vec4 Double  -> Vec4 Double
-- norm_Vec4 (Vec4 ax ay az aw) = let magnitude = sqrt((ax * ax) + (ay * ay) + (az * az) + (aw * aw))
--                            in Vec4 (ax/magnitude) (ay/magnitude) (az/magnitude) (aw/magnitude)

-- mul_Vec3_Num :: (Num a) => Vec3 a -> a -> Vec3 a
-- mul_Vec3_Num (Vec3 x y z) n = Vec3 (x*n) (y*n) (z*n)

-- mul_Vec2_Num :: (Num a) => Vec2 a -> a -> Vec2 a
-- mul_Vec2_Num (Vec2 x y ) n = Vec2 (x*n) (y*n)

-- x_Vec2 :: Vec2 a -> a
-- x_Vec2 (Vec2 x y) = x

-- y_Vec2 :: Vec2 a -> a
-- y_Vec2 (Vec2 x y) = y

-- x_Vec3 :: Vec3 a -> a
-- x_Vec3 (Vec3 x y z) = x

-- y_Vec3 :: Vec3 a -> a
-- y_Vec3 (Vec3 x y z) = y

-- z_Vec3 :: Vec3 a -> a
-- z_Vec3 (Vec3 x y z) = z

-- x_Vec4 :: Vec4 a -> a
-- x_Vec4 (Vec4 x y z w) = x

-- y_Vec4 :: Vec4 a -> a
-- y_Vec4 (Vec4 x y z w) = y

-- z_Vec4 :: Vec4 a -> a
-- z_Vec4 (Vec4 x y z w) = z

-- w_Vec4 :: Vec4 a -> a
-- w_Vec4 (Vec4 x y z w) = w


-- class Selectable f where
--     (!!) :: f a -> Int -> a



-- instance Selectable Vec2  where
--     (Vec2 x y) !! n  = case n of  0 -> x
--                                 1 -> y

-- instance Selectable Vec3  where
--     (Vec3 x y z) !! n  = case n of    0 -> x
--                                     1 -> y
--                                     2 -> z
-- instance Selectable Vec4  where
--     (Vec4 x y z w) !! n  = case n of      0 -> x
--                                         1 -> y
--                                         2 -> z
--                                         3 -> w

-- instance Selectable ((,,) f a) where
--     (a, b, c) !! n  = case n of     0 -> a
--                                     1 -> b
--                                     2 -> c


-- (!!) :: Vec3 a -> Int -> a
-- (Vec3 x y z) !! n  = case n of  0 -> x
--                               1 -> y
--                               2 -> z
-- (!!) :: Vec4 a -> Int -> a
-- (Vec4 x y z w) !! n  = case n of  0 -> x
--                                 1 -> y
--                                 2 -> z
--                                 3 -> w