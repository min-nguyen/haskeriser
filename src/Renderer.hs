{-# LANGUAGE OverloadedStrings #-}

module Renderer
    where

import Prelude hiding (any, mapM_)
import Control.Monad hiding (mapM_)
import Control.Arrow ((***))
import Data.Foldable hiding (elem)
import Data.Maybe
import Data.Word8
import Data.List
import Data.Cross
import Foreign.C.Types
import SDL.Vect
import SDL (($=))
import qualified SDL
import Matrix as Matrix
import SDL_Aux
import Triangle
import Model
import Light
import Camera
import Control.Lens
import Geometry



to_double :: Int -> Double
to_double x = fromIntegral x

--fromMatV4toV3 (viewport_mat * projection_mat *
draw_loop :: Screen -> Model -> Light -> Camera -> IO()
draw_loop screen model light camera = do
    let zbuffer = replicate ((width_i screen)*(height_i screen)) (-100000)
        t_faces = faces model
        t_verts = verts model
        t_norms = norms model
        t_uvs = uvs model
        projection_mat = cam_projection_matrix camera
        viewport_mat = viewport_matrix ((fromIntegral $ width_i screen)/8.0) ((fromIntegral $ height_i screen)/8.0) ((fromIntegral $ width_i screen)*0.75) ((fromIntegral $ height_i screen)*0.75)
    
    --------    Get [(screen coordinates of face vertex, world coordinates of face vertex)] of each face
    screen_world_coords <-  mapM (\ind -> do 
                                   
                                    let face = model_face model ind 
                                        (w_v0, w_v1, w_v2) = mapTuple3 (\i -> model_vert model (fromIntegral $ (face !! i))) (0,1,2)
                           
                                    let screen_coord (V3 a b c) = (( fromMatV4toV3 ( viewport_mat * projection_mat * (fromV3toMatV4 (V3 a b c)) )) :: (V3 Double))
                                        (s_v0, s_v1, s_v2) = mapTuple3 (\v -> screen_coord v)  (w_v0, w_v1, w_v2)
                                    -- print ((s_v0, s_v1, s_v2),  (w_v0, w_v1, w_v2))

                                    return $ (((s_v0, s_v1, s_v2),  (w_v0, w_v1, w_v2)) :: ((V3 Double, V3 Double, V3 Double), (V3 Double, V3 Double, V3 Double)))) ([0 .. (nfaces model) - 1] :: [Int])

    let screen_coords = map (\(x,y) -> x) screen_world_coords   -- :: [(V3 Double, V3 Double, V3 Double)]
        world_coords  = map (\(x,y) -> y) screen_world_coords   -- :: [(V3 Double, V3 Double, V3 Double)]
        process_triangles idx coords = case coords of (x:xs) -> (\(screen_v, world_v) -> do
                                                                            print $ length xs
                                                                            let (world_0, world_1, world_2) = world_v
                                                                                norm = norm_V3 $ or_V3  (world_2 - world_0) (world_1 - world_2)
                                                                                light_intensity = norm * (direction light)
                                                                            when (light_intensity > 0) (do 
                                                                                print $ length xs
                                                                                let screen_v_ints = mapTuple3 (map_V3 floor) screen_v                        
                                                                                    uv = mapTuple3 (model_uv model idx) ((0, 1, 2) :: (Int, Int, Int))
                                                                                draw_triangle screen screen_v_ints uv zbuffer 
                                                                               
                                                                                return ())
                                                                            process_triangles (idx + 1) xs) x
                                                      [] -> return ()

    process_triangles (0::Int) screen_world_coords
    return ()

-- #             Screen ->   Projected 2D Triangle Vertices   ->   UV Coordinates Z-Buffer  -> Updated Z-Buffer                     
draw_triangle :: Screen ->  (V3 Int, V3 Int, V3 Int) ->  (V2 Int, V2 Int, V2 Int) -> [Double] -> IO [Double]
draw_triangle screen screen_vertices uv_vertices zbuffer  = do
    let ((V3 v0x v0y v0z, V3 v1x v1y v1z, V3 v2x v2y v2z)) = screen_vertices
 
    if (v0y == v1y && v0y == v2y) 
        then return zbuffer
        else (do
            let ((V3 v0x v0y v0z, V3 v1x v1y v1z, V3 v2x v2y v2z), (V2 v0u v0v, V2 v1u v1v, V2 v2u v2v)) = order_vertices_i screen_vertices uv_vertices 0
                ((v0, v1, v2), (uv0, uv1, uv2)) = ((V3 v0x v0y v0z, V3 v1x v1y v1z, V3 v2x v2y v2z), (V2 v0u v0v, V2 v1u v1v, V2 v2u v2v))
                triangle_height = v2y - v0y
            v_list <- mapM (\i -> do
                

                    let second_half = (i > (v1y - v0y)) || (v1y == v0y) 
                        segment_height = if second_half then v2y - v1y else v1y - v0y
                        alpha = (to_double i) / (to_double triangle_height)
                        beta = if second_half then (to_double(i - (v1y - v0y))) / (to_double segment_height) else (to_double i)/(to_double segment_height)
                     
                        vA = v0 + (map_V3 floor (mul_V3_Num (map_V3 to_double(v2 - v0)) ( alpha))  )
                        vB = if second_half then v1 + (map_V3 floor (mul_V3_Num (map_V3 to_double (v2 - v1)) beta)) else v0 + (map_V3 floor (mul_V3_Num (map_V3 to_double (v1 - v0)) beta))
                        uvA = uv0 + (map_V2 floor (mul_V2_Num  (map_V2 to_double (uv2 - uv0)) alpha) )  
                        uvB = if second_half then uv1 + (map_V2 floor (mul_V2_Num (map_V2 to_double (uv2 - uv1)) beta)) else uv0 + (map_V2 floor (mul_V2_Num (map_V2 to_double (uv1 - uv0)) beta))
                    -- print $ (second_half, segment_height, alpha, beta, triangle_height, i, v0, v1 ,v2)
                    return $ ((order_min_x_i (vA, vB) (uvA, uvB)  ), i)  ) ([0 .. (triangle_height)] :: [Int])

            zbuffer' <- draw_help v_list zbuffer screen
            return zbuffer'
        )
        


draw_help :: [(((V3 Int, V3 Int), (V2 Int, V2 Int)), Int)] -> [Double] -> Screen -> IO [Double]
draw_help v_list zbuffer screen = case v_list of (x:xs) ->  do
                                                        let ((V3 vAx vAy vAz, V3 vBx vBy vBz), (V2 vAu vAv, V2 vBu vBv)) = fst x    
                                                    
                                                        zbuffer' <- draw_helper v_list (vAx, vBx) vAx screen zbuffer 
                                                
                                                        draw_help xs zbuffer' screen
                                                 []     -> return zbuffer


draw_helper :: [(((V3 Int, V3 Int), (V2 Int, V2 Int)), Int)] -> (Int, Int) -> Int -> Screen -> [Double] -> IO [Double]
draw_helper v_list (start, end) index screen zbuffer =
                                    if index > end
                                        then return zbuffer
                                        else case v_list of (v:vs) ->      do

                                                                        let ((V3 vAx vAy vAz, V3 vBx vBy vBz), (V2 vAu vAv, V2 vBu vBv)) = fst v
                                                                            ((vA', vB'), (uvA', uvB')) =  fst v

                                                                        let phi = if (vBx == vAx) then (1.0 :: Double) else (to_double(index - vAx)) / (to_double( vBx - vAx))
                                                                            (V3 px py pz) = vA' + (map_V3 floor (mul_V3_Num (map_V3 to_double (vB' - vA')) (phi)))
                                                                            (V2 pu pv) = uvA' + (map_V2 floor (mul_V2_Num (map_V2 to_double (uvB' - uvA')) ( phi)))
                                                                            idx = px + py * (width_i screen)
                                                                        -- print (phi, py, vA', vB')
                                                                    
                                                                        if (zbuffer !! (idx)) < (to_double pz)
                                                                            then ( do
                                                                                let zbuffer' = replaceAt (to_double pz) idx zbuffer
                                                                                sdl_put_pixel screen (V2 (fromIntegral $ px) ( fromIntegral $  py)) (get_color Blue)
                                                                                draw_helper vs (start,end) (index + 1) screen zbuffer')
                                                                            else ( do
                                                                            
                                                                                draw_helper vs (start,end) (index + 1) screen zbuffer)
                                                            [] -> return zbuffer
       

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

order_vertices_i :: (V3 Int, V3 Int, V3 Int) -> (V2 Int, V2 Int, V2 Int) -> Int -> ((V3 Int, V3 Int, V3 Int), (V2 Int, V2 Int, V2 Int))
order_vertices_i (V3 v0x v0y v0z, V3 v1x v1y v1z, V3 v2x v2y v2z)  (V2 v0u v0v, V2 v1u v1v, V2 v2u v2v) stage
    | stage == 0 = if (v0y > v1y)   then order_vertices_i (V3 v1x v1y v1z, V3 v0x v0y v0z, V3 v2x v2y v2z)  (V2 v1u v1v, V2 v0u v0v, V2 v2u v2v) 1 
                                    else order_vertices_i (V3 v0x v0y v0z, V3 v1x v1y v1z, V3 v2x v2y v2z)  (V2 v0u v0v, V2 v1u v1v, V2 v2u v2v) 1
    | stage == 1 = if (v0y > v2y)   then order_vertices_i (V3 v2x v2y v2z, V3 v1x v1y v1z, V3 v0x v0y v0z)  (V2 v2u v2v, V2 v1u v1v, V2 v0u v0v) 2
                                    else order_vertices_i (V3 v0x v0y v0z, V3 v1x v1y v1z, V3 v2x v2y v2z)  (V2 v0u v0v, V2 v1u v1v, V2 v2u v2v) 2
    | stage == 2 = if (v1y > v2y)   then order_vertices_i (V3 v0x v0y v0z, V3 v2x v2y v2z, V3 v1x v1y v1z)  (V2 v0u v0v, V2 v2u v2v, V2 v1u v1v) 3
                                    else order_vertices_i (V3 v0x v0y v0z, V3 v1x v1y v1z, V3 v2x v2y v2z)  (V2 v0u v0v, V2 v1u v1v, V2 v2u v2v) 3
    | otherwise = ((V3 v0x v0y v0z, V3 v1x v1y v1z, V3 v2x v2y v2z), (V2 v0u v0v, V2 v1u v1v, V2 v2u v2v))

-- cam_mat = cam_projection_matrix camera
-- viewport_mat = viewport_matrix (fromIntegral $ toInteger $ width screen)/8.0 (fromIntegral $ toInteger $ height screen)/8.0 (fromIntegral $ toInteger $ width screen)*0.75 (fromIntegral $ toInteger $ height screen)*0.75


-- f next_zbuff next_triangles = case next_triangles of (x:xs) -> do 
--                                                             let (va, vb, vc) = points x
--                                                             -- print $ toLists $ cam_matrix * (toMatV4 va) ------- Fix this
--                                                             let v_a = (fromMatV4 $ cam_matrix * (toMatV4 va))
--                                                                 v_b = (fromMatV4 $ cam_matrix * (toMatV4 vb))
--                                                                 v_c = (fromMatV4 $ cam_matrix * (toMatV4 vc))
--                                                             next_zbuff' <- draw_triangle screen (v_a, v_b, v_c) next_zbuff x
--                                                             f next_zbuff' xs 
--                                                      [] -> return ()