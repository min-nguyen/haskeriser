{-# LANGUAGE OverloadedStrings #-}

module Renderer
    where

import Prelude hiding (any, mapM_)
import Control.Monad hiding (mapM_)
import Control.Arrow ((***))
import Control.Monad.Par
import Control.Concurrent
import Control.Monad.Trans.Class 
import Data.Foldable hiding (elem)
import Data.Maybe
import Data.Word8
import Data.List
import Data.Cross
import Foreign.C.Types
import SDL.Vect
import SDL (($=))
import qualified SDL
import Debug.Trace
import SDL_Aux
import Triangle
import Matrix hiding ((!!))
import qualified Matrix  as M ((!!))
import Light
import Camera
import Control.Lens
import Geometry
import qualified Data.Vector as V
import Util
import Data.List.Split
import Rasteriser
import Shader



draw_loop :: Rasteriser -> Shader -> IO()
draw_loop rasteriser shader = do
    let zbuffer = load_zbuffer rasteriser
    --------    Get [(screen coordinates of face vertex, world coordinates of face vertex)] of each face
    screen_world_coords <- mapM (\ind -> do 
                                    let face = model_face (model rasteriser) ind 
                                        (w_v0, w_v1, w_v2) = mapTuple3 (\i -> model_vert (model rasteriser) (fromIntegral $ (face !! i))) (0,1,2)
                                        (s_v0, s_v1, s_v2) = mapTuple3 (\v -> vertex shader rasteriser v)  (w_v0, w_v1, w_v2)

                                    return $ (V3 s_v0 s_v1 s_v2) :: V3 (V3 Double)) ([0 .. (nfaces (model rasteriser)) - 1] :: [Int])

    -- render_screen (screen rasteriser) (process_triangles (0::Int) zbuffer screen_world_coords rasteriser)

    return ()

process_triangles :: Int -> ZBuffer -> [ScreenCoords] -> Rasteriser -> ZBuffer
process_triangles idx zbuff coords (Rasteriser model screen camera light ) = go 
                                            where go = 
                                                     case coords of (x:xs) -> (\(screen_v) -> 
                                                                                
                                                                                let (world_0, world_1, world_2) = Debug.Trace.trace (show (length xs) ++ "/" ++ (show $ length $ faces model) ++ " left to process") world_v
                                                                                    norm = norm_V3 $ or_V3  (world_2 - world_0) (world_1 - world_2)
                                                                                    light_intensity = norm * (direction light)
                                                                                in if (light_intensity > 0) 
                                                                                    then ( 

                                                                                        let screen_v_ints = map_V3 (map_V3 floor) screen_v                        
                                                                                            uv = map_V3 (model_uv model idx) ((0, 1, 2) :: (Int, Int, Int))
                                                                                            zbuff' = draw_triangle (Rasteriser model screen camera light ) screen_v_ints uv zbuff
                                                                                            
                                                                                        in process_triangles (idx + 1) zbuff' xs (Rasteriser model screen camera light )) 
                                                                                    else 
                                                                                        process_triangles (idx + 1) zbuff  xs (Rasteriser model screen camera light )) x
                                                                    empty ->  zbuff
                                   
render_screen :: Screen -> ZBuffer -> IO [()]
render_screen screen zbuffer =  do
                mapM (\index -> do 
                                let px = index `mod` (width_i screen)
                                    py = floor $ (to_double (index - px)) / (to_double (width_i screen)) 
                                    rgba = snd $ zbuffer V.! index
                                sdl_put_pixel screen (V2 (fromIntegral px) ( fromIntegral py)) (rgba)) [0 .. (length zbuffer - 1)]        



-- parallel :: V.Vector (Double, V4 Word8) ->  [[((V3 Double, V3 Double, V3 Double), (V3 Double, V3 Double, V3 Double))]] -> Light -> Model -> Screen ->  V.Vector (Double, V4 Word8)
-- parallel  zbuffer screen_world_coords light model screen  = 
--     runPar $ do
--         [a,b,c,d] <- sequence [new, new, new, new]
--         fork $  put a (process_triangles (0::Int) zbuffer (screen_world_coords !! 0) light model screen)
--         fork $  put b (process_triangles (0::Int) zbuffer (screen_world_coords !! 1) light model screen)
--         fork $  put c (process_triangles (0::Int) zbuffer (screen_world_coords !! 2) light model screen)
--         fork $  put d (process_triangles (0::Int) zbuffer (screen_world_coords !! 3) light model screen)
--         zba <- get a
--         zbb <- get b
--         zbc <- get c
--         zbd <- get d
--         return $ reduce_zbuffer [zba, zbb, zbc, zbd]

-- runPars :: Int
-- runPars =  runPar $ do
--                 [a,b,c,d] <- sequence [new,new,new,new]
--                 fork $ do x <- get a; put b (x+1)
--                 fork $ do x <- get a; put c (x+2)
--                 fork $ do x <- get b; y <- get c; put d (x+y)
--                 fork $ do put a (3 :: Int)
--                 get d


