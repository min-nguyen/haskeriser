

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
import Camera
import Matrix

-- # Triangle Vertices v0 v1 v2 -> Queried Point p -> Barycentric Coordinates
barycentric :: (V2 Double, V2 Double, V2 Double) -> V2 Double -> V3 Double
barycentric (v0, v1, v2) p = if abs b2 < 1 then (V3 (-1) 1 1) else V3 (1 - (b0 + b1)/b2) (b1/b2) (b0/b2)
                        where (b0, b1, b2) = cross3(v2x - v0x, v1x - v0x, v0x - px) (v2y - v0y, v1y - v0y, v0y - py)
                              V2 px  py  = p 
                              V2 v0x v0y = v0
                              V2 v1x v1y = v1 
                              V2 v2x v2y = v2

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


--                 EYE          CENTER        UP                                     
lookat_matrix :: V3 Double -> V3 Double -> V3 Double -> Matrix Double
lookat_matrix eye center up = let   V3 x1 y1 z1 = norm_V3 $ eye - center
                                    V3 x2 y2 z2 = norm_V3 $ or_V3 up (V3 x1 y1 z1)
                                    V3 x3 y3 z3 = norm_V3 $ or_V3 (V3 x1 y1 z1) (V3 x2 y2 z2)
                                    V3 cx cy cz = center
                              in fromList 4 4 [ x1, x3,  x2,   0,
                                                y1, y3,  y2,   0,
                                                z1, z3,  z2,   0,
                                                (-cx), (-cy), (-cz), 1]

forward :: V3 Double
forward = V3 0 0 1

up :: V3 Double
up = V3 0 1 0