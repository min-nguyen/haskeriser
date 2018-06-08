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

draw_loop :: Screen -> Model -> Light -> Camera -> IO()
draw_loop screen model light camera = do
    let zbuffer = replicate ((width_i screen)*(height_i screen)) 0
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
                                        screen_coord (V3 a b c) = ((fromMatV3I (viewport_mat * projection_mat * (toMatV3 (V3 a b c)))) :: (V3 Int))
                                        (s_v0, s_v1, s_v2) = mapTuple3 (\v -> screen_coord v)  (w_v0, w_v1, w_v2)
                                    return $ (((s_v0, s_v1, s_v2),  (w_v0, w_v1, w_v2)) :: ((V3 Int, V3 Int, V3 Int), (V3 Double, V3 Double, V3 Double)))) ([0 .. (nfaces model)] :: [Int])

    let screen_coords = map (\(x,y) -> x) screen_world_coords   -- :: [(V3 Int, V3 Int, V3 Int)]
        world_coords  = map (\(x,y) -> y) screen_world_coords   -- :: [(V3 Double, V3 Double, V3 Double)]
        process_triangles idx = if idx >= nfaces model
                                then return ()
                                else (\(screen, world) -> do
                                        let (world_0, world_1, world_2) = world
                                            norm = norm_V3 $ or_V3  (world_2 - world_0) (world_1 - world_2)
                                            light_intensity = norm * (direction light)
                                        
                                        when (light_intensity > 0) (do 
                                            let uv = map (model_uv model idx) ([0, 1, 2] :: [Int])
                                            -- CALL DRAW FUNCTION HERE --
                                            return ())
                                        
                                        process_triangles (idx + 1) ) (screen_world_coords !! idx)

    process_triangles 0
    --     -- For Each Face Do This
    -- mapM (\(screen, world) -> do
    --         let (world_0, world_1, world_2) = world
    --             norm = norm_V3 $ or_V3  (world_2 - world_0) (world_1 - world_2)
    --             light_intensity = norm * (direction light)
    --         return ()) screen_world_coords
    --         if light_intensity > 0 then let uv = model_uv model  
    return ()

-- # Screen -> Projected 2D Triangle Vertices v0 v1 v2 -> Z-Buffer  -> Triangle  -> Updated Z-Buffer                     
draw_triangle :: Screen ->  (V4 Double, V4 Double, V4 Double) -> [Double] -> Triangle -> IO [Double]
draw_triangle screen projected_vertices zbuffer triangle  = do
    let bound_min = V2 ((fromIntegral $ toInteger $ width screen) - (1.0 :: Double) ) (((fromIntegral $ toInteger $ height screen) - 1.0 ))
        bound_max = V2 (0 :: Double) (0 :: Double)
        clamped = bound_min

        (V4 v0x v0y v0z v0w, V4 v1x v1y v1z v1w, V4 v2x v2y v2z v2w) = projected_vertices
        projected_vert_2D = (V2 v0x v0y, V2 v1x v1y, V2 v2x v2y)

        bbox_min_x =  max 0 (foldr (\(V2 x y) (b) -> (min x  b) ) ((\(V2 xb yb) -> min xb yb) bound_min)  (concat  $ map (^..each) [projected_vert_2D])    )
        bbox_min_y =  max 0 (foldr (\(V2 x y) (b) -> (min y  b) ) ((\(V2 xb yb) -> min xb yb) bound_min) (concat  $ map (^..each) [projected_vert_2D]) )
        bbox_max_x =  min ((\(V2 xb yb) -> xb) (fromIntegral $ toInteger $ width screen)) (foldr (\(V2 x y) (b) -> (max y b) ) ((\(V2 xb yb) -> max xb yb) bound_max) (concat  $ map (^..each) [projected_vert_2D]) )
        bbox_max_y =  min ((\(V2 xb yb) -> yb) (fromIntegral $ toInteger $ height screen)) (foldr (\(V2 x y) (b) -> (max y b) ) ((\(V2 xb yb) -> max xb yb) bound_max) (concat  $ map (^..each) [projected_vert_2D]) )
        
        fillpoints =   [ (px, py, zdepth, fromIntegral zbuffer_index)
                                    | px <- [bbox_min_x .. bbox_max_x], py <- [bbox_min_y .. bbox_max_y], 
                                                let (V3 barx bary barz) = barycentric (V2 v0x v0y, V2 v1x v1y, V2 v2x v2y) (V2 (realToFrac  px) (realToFrac py)),
                                                let zdepth = sum $ zipWith (*)  [v0z, v1z, v2z] [barx, bary, barz],
                                                let zbuffer_index = (floor px + (floor py) * (fromIntegral $ toInteger $ width screen)),
                                                barx >= 0 && bary >=0 && barz >=0 && (zbuffer !! zbuffer_index) < zdepth]
    print (fillpoints)
    sequence $ map (\(px,py,zdepth,zbuffer_index) -> 
                        sdl_put_pixel screen ( V2 (fromInteger px) (fromInteger py) ) (color triangle)) 
                            (map (\(px', py', zdepth', zind') -> (floor px', floor py',floor zdepth',floor zind'))  fillpoints)
    
    let f (zbuffer') fillpoints' = case fillpoints' of
                                    (x:xs) -> let zbuffer'' = (\(px, py, zdepth, zbuffer_index) -> replaceAt zdepth (floor zbuffer_index) (zbuffer') ) x
                                              in f (zbuffer'' :: [Double]) xs
                                    []      -> (zbuffer'  :: [Double])
        zbuffer' = f zbuffer fillpoints
    
    return zbuffer'


    



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