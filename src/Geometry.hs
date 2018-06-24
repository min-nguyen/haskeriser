

{-# LANGUAGE OverloadedStrings #-}

module Geometry
    where

import Prelude
import Prelude hiding (any, mapM_)
import SDL (($=))
import qualified SDL 
import Data.Vec as Vec
import Data.Cross
import Camera
import Matrix
import Types
import Util

-- # Triangle Vertices v0 v1 Vec2 -> Queried Point p -> Barycentric Coordinates
barycentric :: (Vec2 Double, Vec2 Double, Vec2 Double) -> Vec2 Double -> Vec3 Double
barycentric (v0, v1, v2) p = if (abs b2) < 0.025 then (toVec3D (-1.0) 1.0 1.0) else toVec3D (1.0 - (b0 + b1)/b2) (b1/b2) (b0/b2)
                        where  (b0, b1, b2) = cross3(v2x - v0x, v1x - v0x, v0x - px) (v2y - v0y, v1y - v0y, v0y - py)
                               (px , py ) = fromVec2D p 
                               (v0x, v0y) = fromVec2D v0
                               (v1x, v1y) = fromVec2D v1 
                               (v2x, v2y) = fromVec2D v2

projection_matrix :: Camera -> Mat44 Double
projection_matrix cam = Vec.set n3 (toVec4D 0.0 0.0 (-1.0/z) 1.0) identity
                        where  (x, y, z, w) = fromVec4D $ position cam


viewport_matrix :: Double -> Double -> Double -> Double -> Mat44 Double
viewport_matrix x y w h = matFromLists [[w/2.0,   0,         0,          x+w/2.0],
                                        [0,       h/2.0,     0,          y+h/2.0],
                                        [0,       0,         255/2.0,    255/2.0],
                                        [0,       0,         0,          1]]


--                 EYE          CENTER        UP                                     
lookat_matrix :: Vec3 Double -> Vec3 Double -> Vec3 Double -> Mat44 Double
lookat_matrix eye center up = let   (x1, y1, z1) = fromVec3D $  normalize $ eye - center
                                    (x2, y2, z2) = fromVec3D $  normalize $ or_Vec3 up (toVec3 x1 y1 z1)
                                    (x3, y3, z3) = fromVec3D $  normalize $ or_Vec3 (toVec3 x1 y1 z1) (toVec3 x2 y2 z2)
                                    (cx, cy, cz) = fromVec3D $ center
                              in matFromLists [ [x1, x3,  x2,   0],
                                                [y1, y3,  y2,   0],
                                                [z1, z3,  z2,   0],
                                                [(-cx), (-cy), (-cz), 1]]

mvp_matrix :: Shader -> Camera -> Double -> Double -> Double -> Double -> Vec3 Double -> Vec3 Double -> Vec3 Double -> Shader
mvp_matrix shader camera x y w h eye' center' up'  =    let mvp =   (project_shader camera) .
                                                                    (viewport_shader x y w h ) . 
                                                                    (lookat_shader eye' center' up')
                                                        in mvp shader 



project_shader :: Camera -> Shader -> Shader
project_shader  cam  shader = shader {projection = projection_matrix cam}

viewport_shader :: Double -> Double -> Double -> Double ->  Shader -> Shader
viewport_shader  x y w h shader = shader {viewport = viewport_matrix x y w h}

lookat_shader :: Vec3 Double -> Vec3 Double -> Vec3 Double -> Shader -> Shader
lookat_shader  eye center up shader = shader {modelview = lookat_matrix eye center up }

center :: Vec3 Double
center = toVec3D 0.0 0.0 0.0

forward :: Vec3 Double
forward = toVec3D 0.0 0.0 1.0

up :: Vec3 Double
up = toVec3D 0.0 1.0 0.0