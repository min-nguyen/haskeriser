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
import Control.Monad.Par
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
import Geometry
import qualified Data.Vector as V
import Util
import Data.List.Split
import Rasteriser
import Shader
import Model
import Types




draw_loop :: Rasteriser -> Shader -> Mat44 Double -> IO (Rasteriser, Shader)
draw_loop rasteriser shader prev_mvp = do
    let (Rasteriser zbuffer shadowbuffer ambientbuffer model screen camera light) = rasteriser

         ----------- SET UP MVP MATRICES IN SHADER -----------
    shader' <- setup_shader rasteriser shader prev_mvp

    (ras', shade') <- process_triangles rasteriser  shader' 

    return (ras', shade')



process_triangles :: Rasteriser -> Shader -> IO (Rasteriser, Shader)
process_triangles rasteriser shader = foldM f (rasteriser, shader) (getFace (getModel rasteriser))
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