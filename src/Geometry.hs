

{-# LANGUAGE OverloadedStrings #-}

        -- |‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾| -- 
        -- |                                                                        | -- 
        -- |                       RASTERIZER GEOMETRIC ALG                         | -- 
        -- |                                                                        | -- 
        -- |                                                                        | -- 
        --  ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾  -- 

module Geometry
    where

import Prelude hiding (any, mapM_)
import SDL (($=))
import qualified SDL 
import Data.Vec as Vec
import Data.Cross
import Camera
import Types
import Util

barycentric :: (Vec2 Double, Vec2 Double, Vec2 Double) -> Vec2 Double -> Vec3 Double
barycentric (a, b, c) p = let v0 = b - a
                              v1 = c - a
                              v2 = p - a
                              d00 = Vec.dot v0 v0
                              d01 = Vec.dot v0 v1
                              d11 = Vec.dot v1 v1
                              d20 = Vec.dot v2 v0
                              d21 = Vec.dot v2 v1
                              denom = d00 * d11 - d01 * d01
                              v = (d11 * d20 - d01 * d21)/denom
                              w = (d00 * d21 - d01 * d20)/denom
                              u = 1.0 - v - w
                          in (toVec3D u v w)

projection_matrix :: Double -> Mat44 Double
projection_matrix coeff = Vec.set n3 (toVec4D 0.0 0.0 coeff 1.0) identity
                   


viewport_matrix :: Double -> Double -> Double -> Double -> Mat44 Double
viewport_matrix x y w h = matFromLists [[w/2.0,   0,         0,          x+w/2.0],
                                        [0,       h/2.0,     0,          y+h/2.0],
                                        [0,       0,         rCONST_depth/2.0,    rCONST_depth/2.0],
                                        [0,       0,         0,          1]]


--                 EYE          CENTER        UP                                     
lookat_matrix :: Vec3 Double -> Vec3 Double -> Vec3 Double -> Mat44 Double
lookat_matrix eye center up = let   (x1, y1, z1) = fromVec3D $  normalize $ eye - center
                                    (x2, y2, z2) = fromVec3D $  normalize $ cross up (toVec3 x1 y1 z1)
                                    (x3, y3, z3) = fromVec3D $  normalize $ cross (toVec3 x1 y1 z1) (toVec3 x2 y2 z2)
                                    (cx, cy, cz) = fromVec3D $ center
                              in matFromLists [ [x1, x3,  x2,   0],
                                                [y1, y3,  y2,   0],
                                                [z1, z3,  z2,   0],
                                                [(-cx), (-cy), (-cz), 1]]

mvp_matrix :: Shader -> Double -> Double -> Double -> Double -> Double -> Vec3 Double -> Vec3 Double -> Vec3 Double -> Shader
mvp_matrix shader coeff x y w h eye' center' up'  =    let mvp =    (project_shader coeff) .
                                                                    (viewport_shader x y w h ) . 
                                                                    (lookat_shader eye' center' up')
                                                        in mvp shader 



project_shader :: Double -> Shader -> Shader
project_shader  coeff  shader = shader {projection = projection_matrix coeff}

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