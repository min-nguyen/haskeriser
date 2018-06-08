

{-# LANGUAGE OverloadedStrings #-}

module Geometry
    where

import Prelude
import Prelude hiding (any, mapM_)
import SDL.Vect
import SDL (($=))
import qualified SDL
import Data.Matrix as Matrix
import Data.Cross

-- # Triangle Vertices v0 v1 v2 -> Queried Point p -> Barycentric Coordinates
barycentric :: (V2 Double, V2 Double, V2 Double) -> V2 Double -> V3 Double
barycentric (v0, v1, v2) p = if abs b2 < 1 then (V3 (-1) 1 1) else V3 (1 - (b0 + b1)/b2) (b1/b2) (b0/b2)
                        where (b0, b1, b2) = cross3(v2x - v0x, v1x - v0x, v0x - px) (v2y - v0y, v1y - v0y, v0y - py)
                              V2 px  py  = p 
                              V2 v0x v0y = v0
                              V2 v1x v1y = v1 
                              V2 v2x v2y = v2

mul_V3_Num :: (Num a) => V3 a -> a -> V3 a
mul_V3_Num (V3 x y z) n = V3 (x*n) (y*n) (z*n)

mul_V2_Num :: (Num a) => V2 a -> a -> V2 a
mul_V2_Num (V2 x y ) n = V2 (x*n) (y*n)
