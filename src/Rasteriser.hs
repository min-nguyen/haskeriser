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

load_zbuffer :: Rasteriser -> V.Vector (Double, Vec4 Word8)
load_zbuffer (Rasteriser model screen camera  light )  = (V.fromList (replicate ((width_i screen)*(height_i screen)) (-100000.0, Vec4 255 255 255 255))) 

load_shadowbuffer :: Rasteriser -> V.Vector (Double, Vec4 Word8)
load_shadowbuffer (Rasteriser model screen camera  light )  = (V.fromList (replicate ((width_i screen)*(height_i screen)) (-100000.0, Vec4 255 255 255 255))) 




-- #             Screen ->   Projected 2D Triangle Vertices   ->   UV Coordinates Z-Buffer  -> Updated Z-Buffer                     
draw_triangle :: Rasteriser->  Vec3 (Vec3 Int) ->  Vec3 (Vec2 Int) -> ZBuffer ->  ZBuffer
draw_triangle (Rasteriser model screen camera light ) screen_vertices uv_vertices zbuffer  = 
    let (Vec3 (Vec3 v0x v0y v0z) (Vec3 v1x v1y v1z) (Vec3 Vec2x Vec2y Vec2z)  ) = screen_vertices
        -- [ screen_vertices !! i ! j / screen_vertices ! i ! 3 | j <- [0, 1], i <- [0, 1, 2]]
    in if (v0y == v1y && v0y == Vec2y || v0y == Vec2y && v1y == Vec2y || v0y == v1y && Vec2y == v1y) 
        then zbuffer
        else (
            
            -- Order Vectors 
            let (Vec3 v0 v1 Vec2, Vec3 uv0 uv1 uVec2) = order_vertices_i screen_vertices uv_vertices 0
               
            -- Set Triangle Height
                triangle_height = (y_Vec3 Vec2) - (y_Vec3 v0)
            -- Loop from 0 to triangle height    
                v_list = map (\i -> 
                            -- Set Consts
                        let second_half = (i > ( (y_Vec3 v1) -  (y_Vec3 v0))) || ( (y_Vec3 v1) ==  (y_Vec3 v0)) 
                            segment_height = if second_half then  (y_Vec3 Vec2) -  (y_Vec3 v1) else (y_Vec3 v1) -  (y_Vec3 v0)
                            alpha = (to_double i) / (to_double triangle_height)
                            beta = if second_half then (to_double(i - (y_Vec3 v1 - y_Vec3 v0))) / (to_double segment_height) else (to_double i)/(to_double segment_height)
                            -- Set Vectors
                            vA = v0 + (map_Vec3 floor (mul_Vec3_Num (map_Vec3 to_double(Vec2 - v0)) ( alpha))  )
                            vB = if second_half then v1 + (map_Vec3 floor (mul_Vec3_Num (map_Vec3 to_double (Vec2 - v1)) beta)) else v0 + (map_Vec3 floor (mul_Vec3_Num (map_Vec3 to_double (v1 - v0)) beta))
                            uvA = uv0 + (map_Vec2 floor (mul_Vec2_Num  (map_Vec2 to_double (uVec2 - uv0)) alpha) )  
                            uvB = if second_half then uv1 + (map_Vec2 floor (mul_Vec2_Num (map_Vec2 to_double (uVec2 - uv1)) beta)) else uv0 + (map_Vec2 floor (mul_Vec2_Num (map_Vec2 to_double (uv1 - uv0)) beta))
                        -- Return ordered vectors
                        in  ((order_min_x_i (vA, vB) (uvA, uvB)  ), i)  ) ([0 .. (triangle_height - 1)] :: [Int])
            in process_triangle_rows (Rasteriser model screen camera light ) v_list zbuffer        
        )
    
process_triangle_rows :: Rasteriser -> [(((Vec3 Int, Vec3 Int), (Vec2 Int, Vec2 Int)), Int)] -> ZBuffer ->  ZBuffer
process_triangle_rows (Rasteriser model screen camera light ) v_list zbuffer = go 
                                    where go = case v_list of   (x:xs) ->  
                                                                        let ((Vec3 vAx vAy vAz, Vec3 vBx vBy vBz), (Vec2 vAu vAv, Vec2 vBu vBv)) = fst x    
                                                                        -- Set loop from vA.x to vB.x 
                                                                            zbuffer' = draw_triangle_row (Rasteriser model screen camera light ) v_list (vAx, vBx) vAx zbuffer 

                                                                        in process_triangle_rows (Rasteriser model screen camera light ) xs zbuffer'
                                                                []     ->  zbuffer


draw_triangle_row ::  Rasteriser -> [(((Vec3 Int, Vec3 Int), (Vec2 Int, Vec2 Int)), Int)] -> (Int, Int) -> Int -> ZBuffer ->  ZBuffer
draw_triangle_row (Rasteriser model screen camera light ) v_list (start, end) index zbuffer = go
                                     where go =  if index > end
                                                     then  zbuffer
                                                     else case v_list of    (v:vs) ->      
                                                                                    let ((Vec3 vAx vAy vAz, Vec3 vBx vBy vBz), (Vec2 vAu vAv, Vec2 vBu vBv)) = fst v
                                                                                        ((vA, vB), (uvA, uvB)) =  fst v
                                                                                    -- Set phi
                                                                                        phi = if (vBx == vAx) then (1.0 :: Double) else (to_double(index - vAx)) / (to_double( vBx - vAx))
                                                                                    -- Set vector P
                                                                                        (Vec3 px py pz) = vA + (map_Vec3 floor (mul_Vec3_Num (map_Vec3 to_double (vB - vA)) (phi)))
                                                                                    -- Set vector uvP
                                                                                        (Vec2 pu pv) = uvA + (map_Vec2 floor (mul_Vec2_Num (map_Vec2 to_double (uvB - uvA)) ( phi)))
                                                                                    -- Set idx
                                                                                        idx = px + py * (width_i screen)

                                                                                    in if (fst (zbuffer V.! (idx)) < (to_double pz) && px < width_i screen && py < height_i screen && px >= 0 && py >=0)
                                                                                        then ( 
                                                                                            let rgba      = model_diffuse model (Vec2 pu pv)
                                                                                                zbuffer'  = replaceAt (to_double pz, rgba) idx zbuffer
                                                                                                
                                                                                           
                                                                                            in draw_triangle_row (Rasteriser model screen camera light ) vs (start,end) (index + 1) zbuffer')
                                                                                        else ( draw_triangle_row (Rasteriser model screen camera light ) vs (start,end) (index + 1) zbuffer)
                                                                            [] -> zbuffer


order_min_x :: (Vec3 Double, Vec3 Double) -> (Vec2 Double, Vec2 Double) -> ((Vec3 Double, Vec3 Double), (Vec2 Double, Vec2 Double))
order_min_x (Vec3 vAx vAy vAz, Vec3 vBx vBy vBz) (Vec2 vAu vAv, Vec2 vBu vBv)
    | (vAx > vBx) = ((Vec3 vBx vBy vBz, Vec3 vAx vAy vAz), (Vec2 vBu vBv, Vec2 vAu vAv))
    | otherwise   = ((Vec3 vAx vAy vAz, Vec3 vBx vBy vBz), (Vec2 vAu vAv, Vec2 vBu vBv))

order_vertices :: (Vec3 Double, Vec3 Double, Vec3 Double) -> (Vec2 Double, Vec2 Double, Vec2 Double) -> Int ->  ((Vec3 Double, Vec3 Double, Vec3 Double), (Vec2 Double, Vec2 Double, Vec2 Double))
order_vertices (Vec3 v0x v0y v0z, Vec3 v1x v1y v1z, Vec3 Vec2x Vec2y Vec2z)  (Vec2 v0u v0v, Vec2 v1u v1v, Vec2 Vec2u Vec2v) stage
    | stage == 0 = if (v0y > v1y)   then order_vertices (Vec3 v1x v1y v1z, Vec3 v0x v0y v0z, Vec3 Vec2x Vec2y Vec2z)  (Vec2 v1u v1v, Vec2 v0u v0v, Vec2 Vec2u Vec2v) 1 
                                    else order_vertices (Vec3 v0x v0y v0z, Vec3 v1x v1y v1z, Vec3 Vec2x Vec2y Vec2z)  (Vec2 v0u v0v, Vec2 v1u v1v, Vec2 Vec2u Vec2v) 1
    | stage == 1 = if (v0y > Vec2y)   then order_vertices (Vec3 Vec2x Vec2y Vec2z, Vec3 v1x v1y v1z, Vec3 v0x v0y v0z)  (Vec2 Vec2u Vec2v, Vec2 v1u v1v, Vec2 v0u v0v) 2
                                    else order_vertices (Vec3 v0x v0y v0z, Vec3 v1x v1y v1z, Vec3 Vec2x Vec2y Vec2z)  (Vec2 v0u v0v, Vec2 v1u v1v, Vec2 Vec2u Vec2v) 2
    | stage == 2 = if (v1y > Vec2y)   then order_vertices (Vec3 v0x v0y v0z, Vec3 Vec2x Vec2y Vec2z, Vec3 v1x v1y v1z)  (Vec2 v0u v0v, Vec2 Vec2u Vec2v, Vec2 v1u v1v) 3
                                    else order_vertices (Vec3 v0x v0y v0z, Vec3 v1x v1y v1z, Vec3 Vec2x Vec2y Vec2z)  (Vec2 v0u v0v, Vec2 v1u v1v, Vec2 Vec2u Vec2v) 3
    | otherwise = ((Vec3 v0x v0y v0z, Vec3 v1x v1y v1z, Vec3 Vec2x Vec2y Vec2z), (Vec2 v0u v0v, Vec2 v1u v1v, Vec2 Vec2u Vec2v))

order_min_x_i :: (Vec3 Int, Vec3 Int) -> (Vec2 Int, Vec2 Int) -> ((Vec3 Int, Vec3 Int), (Vec2 Int, Vec2 Int))
order_min_x_i (Vec3 vAx vAy vAz, Vec3 vBx vBy vBz) (Vec2 vAu vAv, Vec2 vBu vBv)
    | (vAx > vBx) = ((Vec3 vBx vBy vBz, Vec3 vAx vAy vAz), (Vec2 vBu vBv, Vec2 vAu vAv))
    | otherwise   = ((Vec3 vAx vAy vAz, Vec3 vBx vBy vBz), (Vec2 vAu vAv, Vec2 vBu vBv))

order_vertices_i :: Vec3 (Vec3 Int) -> Vec3 (Vec2 Int) -> Int -> (Vec3 (Vec3 Int), Vec3 (Vec2 Int))
order_vertices_i (Vec3 (Vec3 v0x v0y v0z) (Vec3 v1x v1y v1z) (Vec3 Vec2x Vec2y Vec2z))  (Vec3 (Vec2 v0u v0v) (Vec2 v1u v1v) (Vec2 Vec2u Vec2v)) stage
    | stage == 0 = if (v0y > v1y)   then order_vertices_i (Vec3 (Vec3 v1x v1y v1z) (Vec3 v0x v0y v0z) (Vec3 Vec2x Vec2y Vec2z)) (Vec3 (Vec2 v1u v1v) (Vec2 v0u v0v) (Vec2 Vec2u Vec2v)) 1 
                                    else order_vertices_i (Vec3 (Vec3 v0x v0y v0z) (Vec3 v1x v1y v1z) (Vec3 Vec2x Vec2y Vec2z)) (Vec3 (Vec2 v0u v0v) (Vec2 v1u v1v) (Vec2 Vec2u Vec2v)) 1
    | stage == 1 = if (v0y > Vec2y)   then order_vertices_i (Vec3 (Vec3 Vec2x Vec2y Vec2z) (Vec3 v1x v1y v1z) (Vec3 v0x v0y v0z)) (Vec3 (Vec2 Vec2u Vec2v) (Vec2 v1u v1v) (Vec2 v0u v0v)) 2
                                    else order_vertices_i (Vec3 (Vec3 v0x v0y v0z) (Vec3 v1x v1y v1z) (Vec3 Vec2x Vec2y Vec2z)) (Vec3 (Vec2 v0u v0v) (Vec2 v1u v1v) (Vec2 Vec2u Vec2v)) 2
    | stage == 2 = if (v1y > Vec2y)   then order_vertices_i (Vec3 (Vec3 v0x v0y v0z) (Vec3 Vec2x Vec2y Vec2z) (Vec3 v1x v1y v1z)) (Vec3 (Vec2 v0u v0v) (Vec2 Vec2u Vec2v) (Vec2 v1u v1v)) 3
                                    else order_vertices_i (Vec3 (Vec3 v0x v0y v0z) (Vec3 v1x v1y v1z) (Vec3 Vec2x Vec2y Vec2z)) (Vec3 (Vec2 v0u v0v) (Vec2 v1u v1v) (Vec2 Vec2u Vec2v)) 3
    | otherwise = (Vec3 (Vec3 v0x v0y v0z) (Vec3 v1x v1y v1z) (Vec3 Vec2x Vec2y Vec2z), Vec3 (Vec2 v0u v0v)(Vec2 v1u v1v)(Vec2 Vec2u Vec2v))
                                                                            