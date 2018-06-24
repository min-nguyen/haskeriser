{-# LANGUAGE OverloadedStrings #-}

module Rasteriser
    where

import Prelude
import Prelude hiding (any, mapM_)
import SDL.Vect
import SDL (($=))
import qualified SDL
import Data.Word8
import Data.Vec
import qualified Data.Vector as V
import Data.Cross
import Camera
import Matrix
import Model
import SDL_Aux
import Light
import Geometry
import Util
import Shader
import Data.Vec
import Types

load_zbuffer :: Rasteriser -> V.Vector (Double, Vec4 Word8)
load_zbuffer (Rasteriser model screen camera  light )  = (V.fromList (replicate ((width_i screen)*(height_i screen)) (-100000.0, toVec4 255 255 255 255))) 

load_shadowbuffer :: Rasteriser -> V.Vector (Double, Vec4 Word8)
load_shadowbuffer (Rasteriser model screen camera  light )  = (V.fromList (replicate ((width_i screen)*(height_i screen)) (-100000.0, toVec4 255 255 255 255))) 




-- #             Screen ->  Triangle Vertices   ->  Z-Buffer                     
draw_triangle :: Rasteriser->  Vec3 (Vec4 Int) ->  Int -> Int -> ZBuffer ->  ZBuffer
draw_triangle (Rasteriser model screen camera light ) screen_vertices  px py zbuffer = 
    let 
        -- Screen Coordinates
        [vertex_0, vertex_1, vertex_2] = Vec.toList screen_vertices
        [(x0,y0,z0,w0),(x1,y1,z1,w1),(x2,y2,z2,w2)] = map fromVec4 [vertex_0, vertex_1, vertex_2]

        -- Coordinate Attributes
        c = barycentric (projectVec4to2 (vertex_0 / w0 )) (projectVec4to2 (vertex_1 / w1)) (projectVec4to2 (vertex_2 / w2)) px py  
        z = (z0 * getElem 0 c) + (z1 * getElem 1 c) + (z2 * getElem 2 c) 
        w = (w0 * getElem 0 c) + (w1 * getElem 1 c) + (w2 * getElem 2 c) 
        frag_depth = z/w

    in 
          -- Verify bounds and handle recursion through px and py of lists [0 .. 3] and [0 .. 3]
        if (getElem 0 c < 0 || getElem 1 c < 0 || getElem 2 c < 0 || zbuffer !! (px + py * (width screen)) > frag_depth )
            then recurseVertex zbuffer
            else let fragment_shade shader
                     updated_zbuffer = replaceAt (px + py * (width screen)) frag_depth zbuffer
                     recurseVertex updated_zbuffer
        where
            recurseVertex zbuff = case (px,py) of 
                px > 2 && py >= 2   ->  -- end
                px > 2 && py < 2    ->  draw_triangle (Rasteriser model screen camera light ) screen_vertices  0 (py + 1) zbuff 
                px <= 2             ->  draw_triangle (Rasteriser model screen camera light ) screen_vertices  (px+1) py zbuff             









                
-- order_min_x :: (Vec3 Double, Vec3 Double) -> (Vec2 Double, Vec2 Double) -> ((Vec3 Double, Vec3 Double), (Vec2 Double, Vec2 Double))
-- order_min_x (Vec3 vAx vAy vAz, Vec3 vBx vBy vBz) (Vec2 vAu vAv, Vec2 vBu vBv)
--     | (vAx > vBx) = ((Vec3 vBx vBy vBz, Vec3 vAx vAy vAz), (Vec2 vBu vBv, Vec2 vAu vAv))
--     | otherwise   = ((Vec3 vAx vAy vAz, Vec3 vBx vBy vBz), (Vec2 vAu vAv, Vec2 vBu vBv))

-- order_vertices :: (Vec3 Double, Vec3 Double, Vec3 Double) -> (Vec2 Double, Vec2 Double, Vec2 Double) -> Int ->  ((Vec3 Double, Vec3 Double, Vec3 Double), (Vec2 Double, Vec2 Double, Vec2 Double))
-- order_vertices (Vec3 v0x v0y v0z, Vec3 v1x v1y v1z, Vec3 Vec2x Vec2y Vec2z)  (Vec2 v0u v0v, Vec2 v1u v1v, Vec2 Vec2u Vec2v) stage
--     | stage == 0 = if (v0y > v1y)   then order_vertices (Vec3 v1x v1y v1z, Vec3 v0x v0y v0z, Vec3 Vec2x Vec2y Vec2z)  (Vec2 v1u v1v, Vec2 v0u v0v, Vec2 Vec2u Vec2v) 1 
--                                     else order_vertices (Vec3 v0x v0y v0z, Vec3 v1x v1y v1z, Vec3 Vec2x Vec2y Vec2z)  (Vec2 v0u v0v, Vec2 v1u v1v, Vec2 Vec2u Vec2v) 1
--     | stage == 1 = if (v0y > Vec2y)   then order_vertices (Vec3 Vec2x Vec2y Vec2z, Vec3 v1x v1y v1z, Vec3 v0x v0y v0z)  (Vec2 Vec2u Vec2v, Vec2 v1u v1v, Vec2 v0u v0v) 2
--                                     else order_vertices (Vec3 v0x v0y v0z, Vec3 v1x v1y v1z, Vec3 Vec2x Vec2y Vec2z)  (Vec2 v0u v0v, Vec2 v1u v1v, Vec2 Vec2u Vec2v) 2
--     | stage == 2 = if (v1y > Vec2y)   then order_vertices (Vec3 v0x v0y v0z, Vec3 Vec2x Vec2y Vec2z, Vec3 v1x v1y v1z)  (Vec2 v0u v0v, Vec2 Vec2u Vec2v, Vec2 v1u v1v) 3
--                                     else order_vertices (Vec3 v0x v0y v0z, Vec3 v1x v1y v1z, Vec3 Vec2x Vec2y Vec2z)  (Vec2 v0u v0v, Vec2 v1u v1v, Vec2 Vec2u Vec2v) 3
--     | otherwise = ((Vec3 v0x v0y v0z, Vec3 v1x v1y v1z, Vec3 Vec2x Vec2y Vec2z), (Vec2 v0u v0v, Vec2 v1u v1v, Vec2 Vec2u Vec2v))

-- order_min_x_i :: (Vec3 Int, Vec3 Int) -> (Vec2 Int, Vec2 Int) -> ((Vec3 Int, Vec3 Int), (Vec2 Int, Vec2 Int))
-- order_min_x_i (Vec3 vAx vAy vAz, Vec3 vBx vBy vBz) (Vec2 vAu vAv, Vec2 vBu vBv)
--     | (vAx > vBx) = ((Vec3 vBx vBy vBz, Vec3 vAx vAy vAz), (Vec2 vBu vBv, Vec2 vAu vAv))
--     | otherwise   = ((Vec3 vAx vAy vAz, Vec3 vBx vBy vBz), (Vec2 vAu vAv, Vec2 vBu vBv))

-- order_vertices_i :: Vec3 (Vec3 Int) -> Vec3 (Vec2 Int) -> Int -> (Vec3 (Vec3 Int), Vec3 (Vec2 Int))
-- order_vertices_i (Vec3 (Vec3 v0x v0y v0z) (Vec3 v1x v1y v1z) (Vec3 Vec2x Vec2y Vec2z))  (Vec3 (Vec2 v0u v0v) (Vec2 v1u v1v) (Vec2 Vec2u Vec2v)) stage
--     | stage == 0 = if (v0y > v1y)   then order_vertices_i (Vec3 (Vec3 v1x v1y v1z) (Vec3 v0x v0y v0z) (Vec3 Vec2x Vec2y Vec2z)) (Vec3 (Vec2 v1u v1v) (Vec2 v0u v0v) (Vec2 Vec2u Vec2v)) 1 
--                                     else order_vertices_i (Vec3 (Vec3 v0x v0y v0z) (Vec3 v1x v1y v1z) (Vec3 Vec2x Vec2y Vec2z)) (Vec3 (Vec2 v0u v0v) (Vec2 v1u v1v) (Vec2 Vec2u Vec2v)) 1
--     | stage == 1 = if (v0y > Vec2y)   then order_vertices_i (Vec3 (Vec3 Vec2x Vec2y Vec2z) (Vec3 v1x v1y v1z) (Vec3 v0x v0y v0z)) (Vec3 (Vec2 Vec2u Vec2v) (Vec2 v1u v1v) (Vec2 v0u v0v)) 2
--                                     else order_vertices_i (Vec3 (Vec3 v0x v0y v0z) (Vec3 v1x v1y v1z) (Vec3 Vec2x Vec2y Vec2z)) (Vec3 (Vec2 v0u v0v) (Vec2 v1u v1v) (Vec2 Vec2u Vec2v)) 2
--     | stage == 2 = if (v1y > Vec2y)   then order_vertices_i (Vec3 (Vec3 v0x v0y v0z) (Vec3 Vec2x Vec2y Vec2z) (Vec3 v1x v1y v1z)) (Vec3 (Vec2 v0u v0v) (Vec2 Vec2u Vec2v) (Vec2 v1u v1v)) 3
--                                     else order_vertices_i (Vec3 (Vec3 v0x v0y v0z) (Vec3 v1x v1y v1z) (Vec3 Vec2x Vec2y Vec2z)) (Vec3 (Vec2 v0u v0v) (Vec2 v1u v1v) (Vec2 Vec2u Vec2v)) 3
--     | otherwise = (Vec3 (Vec3 v0x v0y v0z) (Vec3 v1x v1y v1z) (Vec3 Vec2x Vec2y Vec2z), Vec3 (Vec2 v0u v0v)(Vec2 v1u v1v)(Vec2 Vec2u Vec2v))
                                                                            