{-# LANGUAGE OverloadedStrings #-}

module Renderer
    where

import Prelude hiding (any, mapM_)
import Data.Vec as Vec hiding (take, drop, foldr, map, length)
import qualified Data.Vec as Vec (take, drop, foldr, map, length)
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
import Matrix 
import Light
import Camera
import Control.Lens
import Geometry
import qualified Data.Vector as V
import Util
import Data.List.Split
import Rasteriser
import Shader
import Model
import Types

draw_loop :: Rasteriser -> Shader -> IO()
draw_loop rasteriser shader = do
    let (Rasteriser model screen camera light) = rasteriser
        zbuffer         = load_zbuffer rasteriser
        shadowbuffer    = load_shadowbuffer rasteriser

    --------    Get [(screen coordinates of face vertex, world coordinates of face vertex)] of each face
    
    let zbuff' = process_triangles zbuffer rasteriser shader 0
    render_screen (screen) zbuff'
    return ()



process_triangles :: ZBuffer -> Rasteriser -> Shader -> Int -> ZBuffer
process_triangles zbuff rasteriser shader iface = go 
    where go =  if (iface > (nfaces (r_model rasteriser) - 1)) 
                then zbuff 
                else let (zbuff', shader') = process_triangle zbuff rasteriser shader iface (varying_tri shader)
                     in (process_triangles zbuff' rasteriser shader' (iface + 1) )
                     
process_triangle :: ZBuffer -> Rasteriser -> Shader -> Int -> Matrix Double -> (ZBuffer, Shader)
process_triangle zbuff rasteriser shader iface clipc = 
                        let (Rasteriser model screen camera light) = rasteriser

                            -- Acquire shader with new MVP matrices 
                            (screen_coords, new_shader) =   ((project_shader camera) . 
                                                            (viewport_shader screen_width/8 screen_height/8 screen_width * (3/4) screen_height * (3/4)) . 
                                                            (lookat_shader (normalize direction light) center up) $ shader) :: (Vec4 Double, Shader)

                        -- in 
                            ----------- VERTEX SHADER -----------
                            vertex_shader nthvert t_vertices t_shader = if iface > 2 then (t_vertices, t_shader)
                                                                        else let (vertex', shader') = vertex_shade shader model iface nthvert
                                                                             in  vertex_shader (nthvert + 1) (vertex' : t_vertices) shader'

                            ([vertex_x, vertex_y, vertex_z], shader') = (vertex_shader 0 [] new_shader) ::  ([Vec4 Double], Shader)
                            (triangle_vertices', shader'') = (Vec.fromList [vertex_x, vertex_y, vertex_z], shader') :: ((Vec3 (Vec4 Double)), Shader)

                            ----------- * TRIANGLE * -----------
                            -----------   SET BBOX -----------
                            bboxmin = foldr (\(x, y) (x', y') -> ((min x x'),(min y y')) )  ((-1000000.0), (-1000000.0)) [ (  ((triangle_vertices' M.!! i ) M.!! 0), (minimum $ V.toList (getCol i pts2))  ) |  i <- [0,1,2]]
                            bboxmax = foldr (\(x, y) (x', y') -> ((max x x'),(max y y')) )  ((1000000.0), (1000000.0))   [ (  ((triangle_vertices' M.!! i ) M.!! 0), (maximum $ V.toList (getCol i pts2))   ) |  i <- [0,1,2]]
                        
                            --------------------------------------
                            
                            update_zbuffer px py zbuffer = let  zbuffer' = draw_triangle px py pts2 zbuff rasteriser shader''
                                                                (px', py') = if not (py > snd bboxmax && px <= fst bboxmax) 
                                                                                then (px, py + 1) 
                                                                                else if (py > snd bboxmax) && (px <= fst bboxmax) 
                                                                                    then (px + 1, snd bboxmin ) 
                                                                                    else (px, py)
                                                            in  if px' == px && py' == py 
                                                                then zbuffer' 
                                                                else update_zbuffer px' py' zbuffer'
                                                
                            zbuffer = update_zbuffer (fst bboxmin) (snd bboxmin) zbuff

                        in  (zbuffer, shader'')

render_screen :: Screen -> ZBuffer -> IO [()]
render_screen screen zbuffer =  do
                mapM (\index -> do 
                                let px = index `mod` (width_i screen)
                                    py = floor $ (to_double (index - px)) / (to_double (width_i screen)) 
                                    rgba = snd $ zbuffer V.! index
                                sdl_put_pixel screen (V2 (fromIntegral px) ( fromIntegral py)) (rgba)) [0 .. (length zbuffer - 1)]        



-- parallel :: V.Vector (Double, Vec4 Word8) ->  [[((Vec3 Double, Vec3 Double, Vec3 Double), (Vec3 Double, Vec3 Double, Vec3 Double))]] -> Light -> Model -> Screen ->  V.Vector (Double, Vec4 Word8)
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


-- pts = Matrix.transpose $ viewport_mat shader * clipc
-- pts2 = (scaleMatrix (1/(Matrix.getElem 3 0 pts)) (colVector (getCol 0 pts)) ) <|> 
--         (scaleMatrix (1/(Matrix.getElem 3 1 pts)) (colVector (getCol 1 pts)) ) <|>  
--             (scaleMatrix (1/(Matrix.getElem 3 2 pts)) (colVector (getCol 2 pts)) )