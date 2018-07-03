{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

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
import Control.Monad.Par as Par
import Control.Concurrent
import Control.Monad.Trans.Class 
import Data.Foldable hiding (elem)
import Data.Maybe
import Data.Word8
import Debug.Trace as Trace
import Data.List as List
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
import Control.Parallel
import Geometry
import qualified Data.Vector as V
import Util
import Data.List.Split
import Rasteriser
import Shader
import Model
import Types
import Control.Parallel.Strategies

pardraw :: Rasteriser -> Shader ->  (Rasteriser, Shader)
pardraw rasteriser shader  =  runPar ( do
    let chunk_total = getNumFaces $ getModel rasteriser
        chunk_size = floor ((to_double chunk_total)/2.0)
    f1 <-  (spawn (process_triangles rasteriser  shader 0 (chunk_size*1 - 1)))
    f2 <-  (spawn (process_triangles rasteriser  shader (chunk_size*1)(chunk_size - 1)))
    -- f3 <-  (spawn (process_triangles rasteriser  shader (chunk_size*2)(chunk_size*3 - 1)))
    -- f4 <-  (spawn (process_triangles rasteriser  shader (chunk_size*3)(chunk_total - 1)))
    (ras1, shade1) <- Par.get f1
    (ras2, shade2) <- Par.get f2
    -- (ras3, shade3) <- Par.get f3
    -- (ras4, shade4) <- Par.get f4
    -- let strategy = myStrat reduce
    case shader of  (CameraShader {..}) ->           (do 
                                                    let zbuffer = reduceBuffers (V.toList (getZBuffer ras1)) (V.toList (getZBuffer ras2))  --(getZBuffer ras3) (getZBuffer ras4)
                                                    return (ras2 {getZBuffer = (V.fromList zbuffer)}, shade2))
                    (DirectionalLightShader {..}) -> (do 
                                                    let dbuffer = reduceBuffers  (V.toList (getDepthBuffer ras1)) (V.toList (getDepthBuffer ras2)) 
                                                    return (ras2 {getDepthBuffer = (V.fromList dbuffer)}, shade2) )                        
    )

reduceBuffers :: [(Double, Vec.Vec4 Word8) ] -> [(Double, Vec.Vec4 Word8) ] -> [(Double, Vec.Vec4 Word8) ]
reduceBuffers [x] [t] = [par2_reduce x t]
reduceBuffers xs ts  = 
    let len = length xs
        ((xs1, xs2), (ts1, ts2)) = mapTuple2 (splitAt (len `div` 2)) (xs,ts)

    in (par2 reduceBuffers xs1 ts1 ) ++ (par2 reduceBuffers xs2 ts2)


draw_loop :: Rasteriser -> Shader  -> IO (Rasteriser, Shader)
draw_loop rasteriser shader  = do
    let (Rasteriser zbuffer shadowbuffer ambientbuffer model screen camera light) = rasteriser
   
         ----------- SET UP MVP MATRICES IN SHADER -----------

    let (ras', shade') = pardraw rasteriser  shader

    return (ras', shade')

process_triangles :: Rasteriser -> Shader -> Int -> Int -> Par (Rasteriser, Shader)
process_triangles rasteriser shader start_index end_index =  foldM f (rasteriser, shader) (take ((getNumFaces $ getModel rasteriser) - end_index) (drop start_index $ getFace (getModel rasteriser)))
    where f = \(ras', shader') face -> (process_triangle ras' shader' face)


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