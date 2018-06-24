

{-# LANGUAGE OverloadedStrings #-}

module Geometry
    where

import Prelude
import Prelude hiding (any, mapM_)
import SDL.Vect
import SDL (($=))
import qualified SDL
import Data.Vec
import Data.Cross
import Camera
import Matrix
import Types

-- # Triangle Vertices v0 v1 Vec2 -> Queried Point p -> Barycentric Coordinates
barycentric :: (Vec2 Double, Vec2 Double, Vec2 Double) -> Vec2 Double -> Vec3 Double
barycentric (v0, v1, Vec2) p = if abs b2 < 1 then (Vec3 (-1) 1 1) else Vec3 (1 - (b0 + b1)/b2) (b1/b2) (b0/b2)
                        where (b0, b1, b2) = cross3(Vec2x - v0x, v1x - v0x, v0x - px) (Vec2y - v0y, v1y - v0y, v0y - py)
                              Vec2 px  py  = p 
                              Vec2 v0x v0y = v0
                              Vec2 v1x v1y = v1 
                              Vec2 Vec2x Vec2y = Vec2

projection_matrix :: Camera -> Vec4 (Vec4 Double)
projection_matrix cam = set n3 (fromList [0, 0, -1/z, 1]) identity
                        where Vec4 x y z w = position cam


viewport_matrix :: Double -> Double -> Double -> Double -> Vec4 (Vec4 Double)
viewport_matrix x y w h = fromList 4 4 [w/2.0,   0,         0,          x+w/2.0,
                                        0,       h/2.0,     0,          y+h/2.0,
                                        0,       0,         255/2.0,    255/2.0,
                                        0,       0,         0,          1]


--                 EYE          CENTER        UP                                     
lookat_matrix :: Vec3 Double -> Vec3 Double -> Vec3 Double -> Vec4 (Vec4 Double)
lookat_matrix eye center up = let   Vec3 x1 y1 z1 = norm_Vec3 $ eye - center
                                    Vec3 x2 y2 z2 = norm_Vec3 $ or_Vec3 up (Vec3 x1 y1 z1)
                                    Vec3 x3 y3 z3 = norm_Vec3 $ or_Vec3 (Vec3 x1 y1 z1) (Vec3 x2 y2 z2)
                                    Vec3 cx cy cz = center
                              in fromList 4 4 [ x1, x3,  x2,   0,
                                                y1, y3,  y2,   0,
                                                z1, z3,  z2,   0,
                                                (-cx), (-cy), (-cz), 1]

forward :: Vec3 Double
forward = Vec3 0 0 1

up :: Vec3 Double
up = Vec3 0 1 0