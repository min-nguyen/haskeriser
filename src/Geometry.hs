

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

barycentric :: (Vec2 Double, Vec2 Double, Vec2 Double) -> Vec2 Double -> Maybe (Vec3 Double)
barycentric (a, b, c) p =   let ((ax, ay), (bx, by), (cx, cy), (px, py)) = mapTuple4 ((fromVec2)) (a, b, c, p)
                                getX = getElem 0
                                getY = getElem 1

                                sx = toVec3 (getX c - getX a) (getX b - getX a)  (getX a - getX p) 
                                sy = toVec3 (getY c - getY a) (getY b - getY a)  (getY a - getY p) 
                                (ux, uy, uz) = fromVec3 (cross sx sy)
                        
                                (w,u,v) = ((1.0 - (ux + uy)/uz), (uy/uz), (ux/uz))

                            in  (if (u > 0.0 && v > 0.0 && u + v < 1.0 && 0 <= w)
                                then (Just (toVec3D (1.0 - (ux + uy)/uz) (uy/uz) (ux/uz)))
                                else Nothing)

mvp_matrix :: Shader -> Shader
mvp_matrix shader = shader { getMVP = (multmm (getViewport shader) (multmm (getProjection shader) (getModelView shader))) }

projection_matrix :: Double -> Mat44 Double
projection_matrix coeff = Vec.set n3 (toVec4D 0.0 0.0 coeff 1.0) identity
                   

viewport_matrix :: Double -> Double -> Double -> Double -> Mat44 Double
viewport_matrix x y w h =             matFromLists [[w/2.0,   0,         0,                 x+w/2.0],
                                                    [0,       h/2.0,     0,                 y+h/2.0],
                                                    [0,       0,         rCONST_depth/2.0,  rCONST_depth/2.0],
                                                    [0,       0,         0,                 1.0]]


--                 EYE          CENTER        UP                                     
lookat_matrix :: Vec3 Double -> Vec3 Double -> Vec3 Double -> Mat44 Double
lookat_matrix = rotationLookAt

mk_transformation_matrix :: Num a => Mat33 a -> Vec3 a -> Mat44 a
mk_transformation_matrix r t = 
        (toVec4 (snoc3 r1 tx) (snoc3 r2 ty) (snoc3 r3 tz) (toVec4 0 0 0 1))
    where snoc3 v3 = (\w -> let (x, y, z) = fromVec3 v3 in toVec4 x y z w)
          (tx, ty, tz) = fromVec3 t
          (r1, r2, r3) = fromVec3 r


mvp_shader :: Shader -> Double -> Double -> Double -> Double -> Double -> Vec3 Double -> Vec3 Double -> Vec3 Double -> Shader
mvp_shader shader coeff x y w h eye' center' up'  =     let mvpshader =  ((viewport_shader x y w h ) . 
                                                                          (project_shader coeff) .
                                                                          (lookat_shader eye' center' up')) shader
                                                            mvpshader' = mvp_matrix mvpshader
                                                        in  mvpshader'

project_shader :: Double -> Shader -> Shader
project_shader  coeff  shader = shader {getProjection = projection_matrix coeff}

viewport_shader :: Double -> Double -> Double -> Double ->  Shader -> Shader
viewport_shader  x y w h shader = shader {getViewport = viewport_matrix x y w h}

lookat_shader :: Vec3 Double -> Vec3 Double -> Vec3 Double -> Shader -> Shader
lookat_shader  eyev centerv upv shader = shader {getModelView = lookat_matrix eyev centerv upv }

center :: Vec3 Double
center = toVec3D 0.0 0.0 0.0

eye :: Vec3 Double
eye = toVec3D 1.0 1.0 4.0

up :: Vec3 Double
up = toVec3D 0.0 1.0 0.0 