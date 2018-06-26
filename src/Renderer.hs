{-# LANGUAGE OverloadedStrings #-}

        -- |‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾| -- 
        -- |                                                                        | -- 
        -- |                        ROOT RENDERING SOURCE                           | -- 
        -- |                                                                        | -- 
        -- |                                                                        | -- 
        --  ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾  -- 

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
import Debug.Trace as Trace
import Data.List
import Data.Cross
import Foreign.C.Types
import SDL.Vect
import SDL (($=))
import qualified SDL
import SDLx
import Triangle
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




draw_loop :: Rasteriser -> Shader -> Maybe (Mat44 Double) -> IO (Rasteriser, Shader)
draw_loop rasteriser shader prev_mvp = do
    let (Rasteriser zbuffer shadowbuffer model screen camera light) = rasteriser

        screen_width    = to_double $ width_i screen
        screen_height   = to_double $ height_i screen
        light_dir       = Vec.normalize $ direction light
        depth_coeff     = 0.0

         ----------- SET UP MVP MATRICES IN SHADER -----------
    shader' <- setup_shader rasteriser shader prev_mvp

    let (ras', shade') = process_triangles rasteriser  shader' 0
    
    -- render_screen ras'

    return (ras', shade')



process_triangles :: Rasteriser -> Shader -> Int -> (Rasteriser, Shader)
process_triangles rasteriser shader iface = go 
    where go =  if (iface > (getNumFaces (getModel rasteriser) - 1)) 
                then (rasteriser, shader)
                else let (rasteriser', shader') = (process_triangle rasteriser shader iface)
                     in (process_triangles rasteriser' shader' (iface + 1) )
                     
process_triangle :: Rasteriser -> Shader -> Int -> (Rasteriser, Shader)
process_triangle rasteriser shader iface  = 
                        let ----------- VERTEX SHADER -----------
                            (zeroth_vertex, zeroth_shader) = vertex_shade shader ( rasteriser)  iface 0 
                            (vertexes, shader') = (foldr (\nth_vertex (vert_coords, folded_shader) -> (let (vs, folded_shader') = vertex_shade folded_shader ( rasteriser) iface nth_vertex  :: ( (Vec4 Double), Shader)
                                                                                                       in  (vs:vert_coords, folded_shader'))) (zeroth_vertex:[], zeroth_shader) [1, 2]) :: ( [Vec4 Double], Shader)
                                                                         

                            (vertex_x:vertex_y:vertex_z:_) = vertexes

                      
                            (screen_coordinates, shader'') =  (toVec3 vertex_x vertex_y vertex_z, shader')  :: (Mat34 Double, Shader)
                 
                            ----------- * TRIANGLE * -----------

                            -----------   SET BBOX -----------

                            fetchx i = getElem 0 (getElem i screen_coordinates )
                            fetchy i = getElem 1 (getElem i screen_coordinates )
                            fetchw i = getElem 3 (getElem i screen_coordinates )

                            bboxmin = foldr (\(x, y) (x', y') -> ((min x x'),(min y y')) )  
                                            (1000000.0, 1000000.0)
                                            [ (  (fetchx i)/(fetchw i) ,   (fetchy i)/(fetchw i)  ) |  i <- [0,1,2] ]

                            bboxmax = foldr (\(x, y) (x', y') -> ((max x x'),(max y y')) )  
                                            ((-1000000.0), (-1000000.0))   
                                            [ (  (fetchx i)/(fetchw i) ,   (fetchy i)/(fetchw i) ) |  i <- [0,1,2] ]
                        
                            --------------------------------------
                            
                            (updated_rasteriser, updated_shader) =   (draw_triangle rasteriser shader'' screen_coordinates  (floor $ fst bboxmin, floor $ fst bboxmax) 
                                                                                                                                                (floor $ snd bboxmin, floor $ snd bboxmax) 
                                                                                                                                                (floor $ fst bboxmin) 
                                                                                                                                                (floor $ snd bboxmin))
                                                
                           

                        in  (updated_rasteriser, updated_shader)

render_screen :: Rasteriser -> IO [()]
render_screen ras =
                mapM (\index -> do 
                                let screen = getScreen ras
                                    zbuffer = getZBuffer ras
                                    px = index `mod` (width_i screen)
                                    py = floor $ (to_double (index - px)) / (to_double (width_i screen)) 
                                    rgba = vec4ToV4 $ snd $ zbuffer V.! index
                                
                                debug ((px,py)) (sdl_put_pixel screen (V2 (fromIntegral px) ( fromIntegral py)) (rgba))) [0 .. (length (getZBuffer ras) - 1)]        



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