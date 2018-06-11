{-# LANGUAGE OverloadedStrings #-}

module Rasteriser
    where

import Prelude
import Prelude hiding (any, mapM_)
import SDL.Vect
import SDL (($=))
import qualified SDL
import Data.Word8
import qualified Data.Vector as V
import Data.Matrix as Matrix
import Data.Cross
import Camera
import Matrix
import Model
import SDL_Aux
import Light
import Geometry
import Util

data Rasteriser = Rasteriser {  model       :: Model,
                                screen      :: Screen,
                                camera      :: Camera,
                                light       :: Light}

type ZBuffer = V.Vector (Double, V4 Word8)

type ScreenCoords = V3 (V3 Int) 

load_zbuffer :: Rasteriser -> V.Vector (Double, V4 Word8)
load_zbuffer (Rasteriser model screen camera  light )  = (V.fromList (replicate ((width_i screen)*(height_i screen)) (-100000.0, V4 255 255 255 255))) 


-- #             Screen ->   Projected 2D Triangle Vertices   ->   UV Coordinates Z-Buffer  -> Updated Z-Buffer                     
draw_triangle :: Rasteriser->  V3 (V3 Int) ->  V3 (V2 Int) -> ZBuffer ->  ZBuffer
draw_triangle (Rasteriser model screen camera light ) screen_vertices uv_vertices zbuffer  = 
    let (V3 (V3 v0x v0y v0z) (V3 v1x v1y v1z) (V3 v2x v2y v2z)  ) = screen_vertices
        -- [ screen_vertices !! i ! j / screen_vertices ! i ! 3 | j <- [0, 1], i <- [0, 1, 2]]
    in if (v0y == v1y && v0y == v2y || v0y == v2y && v1y == v2y || v0y == v1y && v2y == v1y) 
        then zbuffer
        else (
            
            -- Order Vectors 
            let (V3 v0 v1 v2, V3 uv0 uv1 uv2) = order_vertices_i screen_vertices uv_vertices 0
               
            -- Set Triangle Height
                triangle_height = (y_V3 v2) - (y_V3 v0)
            -- Loop from 0 to triangle height    
                v_list = map (\i -> 
                            -- Set Consts
                        let second_half = (i > ( (y_V3 v1) -  (y_V3 v0))) || ( (y_V3 v1) ==  (y_V3 v0)) 
                            segment_height = if second_half then  (y_V3 v2) -  (y_V3 v1) else (y_V3 v1) -  (y_V3 v0)
                            alpha = (to_double i) / (to_double triangle_height)
                            beta = if second_half then (to_double(i - (y_V3 v1 - y_V3 v0))) / (to_double segment_height) else (to_double i)/(to_double segment_height)
                            -- Set Vectors
                            vA = v0 + (map_V3 floor (mul_V3_Num (map_V3 to_double(v2 - v0)) ( alpha))  )
                            vB = if second_half then v1 + (map_V3 floor (mul_V3_Num (map_V3 to_double (v2 - v1)) beta)) else v0 + (map_V3 floor (mul_V3_Num (map_V3 to_double (v1 - v0)) beta))
                            uvA = uv0 + (map_V2 floor (mul_V2_Num  (map_V2 to_double (uv2 - uv0)) alpha) )  
                            uvB = if second_half then uv1 + (map_V2 floor (mul_V2_Num (map_V2 to_double (uv2 - uv1)) beta)) else uv0 + (map_V2 floor (mul_V2_Num (map_V2 to_double (uv1 - uv0)) beta))
                        -- Return ordered vectors
                        in  ((order_min_x_i (vA, vB) (uvA, uvB)  ), i)  ) ([0 .. (triangle_height - 1)] :: [Int])
            in process_triangle_rows (Rasteriser model screen camera light ) v_list zbuffer        
        )
    
process_triangle_rows :: Rasteriser -> [(((V3 Int, V3 Int), (V2 Int, V2 Int)), Int)] -> ZBuffer ->  ZBuffer
process_triangle_rows (Rasteriser model screen camera light ) v_list zbuffer = go 
                                    where go = case v_list of   (x:xs) ->  
                                                                        let ((V3 vAx vAy vAz, V3 vBx vBy vBz), (V2 vAu vAv, V2 vBu vBv)) = fst x    
                                                                        -- Set loop from vA.x to vB.x 
                                                                            zbuffer' = draw_triangle_row (Rasteriser model screen camera light ) v_list (vAx, vBx) vAx zbuffer 

                                                                        in process_triangle_rows (Rasteriser model screen camera light ) xs zbuffer'
                                                                []     ->  zbuffer


draw_triangle_row ::  Rasteriser -> [(((V3 Int, V3 Int), (V2 Int, V2 Int)), Int)] -> (Int, Int) -> Int -> ZBuffer ->  ZBuffer
draw_triangle_row (Rasteriser model screen camera light ) v_list (start, end) index zbuffer = go
                                     where go =  if index > end
                                                     then  zbuffer
                                                     else case v_list of    (v:vs) ->      
                                                                                    let ((V3 vAx vAy vAz, V3 vBx vBy vBz), (V2 vAu vAv, V2 vBu vBv)) = fst v
                                                                                        ((vA, vB), (uvA, uvB)) =  fst v
                                                                                    -- Set phi
                                                                                        phi = if (vBx == vAx) then (1.0 :: Double) else (to_double(index - vAx)) / (to_double( vBx - vAx))
                                                                                    -- Set vector P
                                                                                        (V3 px py pz) = vA + (map_V3 floor (mul_V3_Num (map_V3 to_double (vB - vA)) (phi)))
                                                                                    -- Set vector uvP
                                                                                        (V2 pu pv) = uvA + (map_V2 floor (mul_V2_Num (map_V2 to_double (uvB - uvA)) ( phi)))
                                                                                    -- Set idx
                                                                                        idx = px + py * (width_i screen)

                                                                                    in if (fst (zbuffer V.! (idx)) < (to_double pz) && px < width_i screen && py < height_i screen && px >= 0 && py >=0)
                                                                                        then ( 
                                                                                            let rgba      = model_diffuse model (V2 pu pv)
                                                                                                zbuffer'  = replaceAt (to_double pz, rgba) idx zbuffer
                                                                                                
                                                                                           
                                                                                            in draw_triangle_row (Rasteriser model screen camera light ) vs (start,end) (index + 1) zbuffer')
                                                                                        else ( draw_triangle_row (Rasteriser model screen camera light ) vs (start,end) (index + 1) zbuffer)
                                                                            [] -> zbuffer


order_min_x :: (V3 Double, V3 Double) -> (V2 Double, V2 Double) -> ((V3 Double, V3 Double), (V2 Double, V2 Double))
order_min_x (V3 vAx vAy vAz, V3 vBx vBy vBz) (V2 vAu vAv, V2 vBu vBv)
    | (vAx > vBx) = ((V3 vBx vBy vBz, V3 vAx vAy vAz), (V2 vBu vBv, V2 vAu vAv))
    | otherwise   = ((V3 vAx vAy vAz, V3 vBx vBy vBz), (V2 vAu vAv, V2 vBu vBv))

order_vertices :: (V3 Double, V3 Double, V3 Double) -> (V2 Double, V2 Double, V2 Double) -> Int ->  ((V3 Double, V3 Double, V3 Double), (V2 Double, V2 Double, V2 Double))
order_vertices (V3 v0x v0y v0z, V3 v1x v1y v1z, V3 v2x v2y v2z)  (V2 v0u v0v, V2 v1u v1v, V2 v2u v2v) stage
    | stage == 0 = if (v0y > v1y)   then order_vertices (V3 v1x v1y v1z, V3 v0x v0y v0z, V3 v2x v2y v2z)  (V2 v1u v1v, V2 v0u v0v, V2 v2u v2v) 1 
                                    else order_vertices (V3 v0x v0y v0z, V3 v1x v1y v1z, V3 v2x v2y v2z)  (V2 v0u v0v, V2 v1u v1v, V2 v2u v2v) 1
    | stage == 1 = if (v0y > v2y)   then order_vertices (V3 v2x v2y v2z, V3 v1x v1y v1z, V3 v0x v0y v0z)  (V2 v2u v2v, V2 v1u v1v, V2 v0u v0v) 2
                                    else order_vertices (V3 v0x v0y v0z, V3 v1x v1y v1z, V3 v2x v2y v2z)  (V2 v0u v0v, V2 v1u v1v, V2 v2u v2v) 2
    | stage == 2 = if (v1y > v2y)   then order_vertices (V3 v0x v0y v0z, V3 v2x v2y v2z, V3 v1x v1y v1z)  (V2 v0u v0v, V2 v2u v2v, V2 v1u v1v) 3
                                    else order_vertices (V3 v0x v0y v0z, V3 v1x v1y v1z, V3 v2x v2y v2z)  (V2 v0u v0v, V2 v1u v1v, V2 v2u v2v) 3
    | otherwise = ((V3 v0x v0y v0z, V3 v1x v1y v1z, V3 v2x v2y v2z), (V2 v0u v0v, V2 v1u v1v, V2 v2u v2v))

order_min_x_i :: (V3 Int, V3 Int) -> (V2 Int, V2 Int) -> ((V3 Int, V3 Int), (V2 Int, V2 Int))
order_min_x_i (V3 vAx vAy vAz, V3 vBx vBy vBz) (V2 vAu vAv, V2 vBu vBv)
    | (vAx > vBx) = ((V3 vBx vBy vBz, V3 vAx vAy vAz), (V2 vBu vBv, V2 vAu vAv))
    | otherwise   = ((V3 vAx vAy vAz, V3 vBx vBy vBz), (V2 vAu vAv, V2 vBu vBv))

order_vertices_i :: V3 (V3 Int) -> V3 (V2 Int) -> Int -> (V3 (V3 Int), V3 (V2 Int))
order_vertices_i (V3 (V3 v0x v0y v0z) (V3 v1x v1y v1z) (V3 v2x v2y v2z))  (V3 (V2 v0u v0v) (V2 v1u v1v) (V2 v2u v2v)) stage
    | stage == 0 = if (v0y > v1y)   then order_vertices_i (V3 (V3 v1x v1y v1z) (V3 v0x v0y v0z) (V3 v2x v2y v2z)) (V3 (V2 v1u v1v) (V2 v0u v0v) (V2 v2u v2v)) 1 
                                    else order_vertices_i (V3 (V3 v0x v0y v0z) (V3 v1x v1y v1z) (V3 v2x v2y v2z)) (V3 (V2 v0u v0v) (V2 v1u v1v) (V2 v2u v2v)) 1
    | stage == 1 = if (v0y > v2y)   then order_vertices_i (V3 (V3 v2x v2y v2z) (V3 v1x v1y v1z) (V3 v0x v0y v0z)) (V3 (V2 v2u v2v) (V2 v1u v1v) (V2 v0u v0v)) 2
                                    else order_vertices_i (V3 (V3 v0x v0y v0z) (V3 v1x v1y v1z) (V3 v2x v2y v2z)) (V3 (V2 v0u v0v) (V2 v1u v1v) (V2 v2u v2v)) 2
    | stage == 2 = if (v1y > v2y)   then order_vertices_i (V3 (V3 v0x v0y v0z) (V3 v2x v2y v2z) (V3 v1x v1y v1z)) (V3 (V2 v0u v0v) (V2 v2u v2v) (V2 v1u v1v)) 3
                                    else order_vertices_i (V3 (V3 v0x v0y v0z) (V3 v1x v1y v1z) (V3 v2x v2y v2z)) (V3 (V2 v0u v0v) (V2 v1u v1v) (V2 v2u v2v)) 3
    | otherwise = (V3 (V3 v0x v0y v0z) (V3 v1x v1y v1z) (V3 v2x v2y v2z), V3 (V2 v0u v0v)(V2 v1u v1v)(V2 v2u v2v))
                                                                            