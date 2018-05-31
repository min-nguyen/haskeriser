{-# LANGUAGE OverloadedStrings #-}

module Renderer
    where

import Prelude hiding (any, mapM_)
import Control.Monad hiding (mapM_)
import Data.Foldable hiding (elem)
import Data.Maybe
import Data.Word8
import Data.Cross
import Foreign.C.Types
import SDL.Vect
import SDL (($=))
import qualified SDL
import qualified GLM as GLM
import SDL_Aux
import Scene

-- # Triangle Vertices v0 v1 v2 -> Queried Point p -> Barycentric Coordinates
barycentric :: (V2 Double, V2 Double, V2 Double) -> V2 Double -> V3 Double
barycentric (v0, v1, v2) p = if abs b2 < 1 then (V3 (-1) 1 1) else V3 (1 - (b0 + b1)/b2) (b1/b2) (b0/b2)
                        where (b0, b1, b2) = cross3(v2x - v0x, v1x - v0x, v0x - px) (v2y - v0y, v1y - v0y, v0y - py)
                              V2 px  py  = p 
                              V2 v0x v0y = v0
                              V2 v1x v1y = v1 
                              V2 v2x v2y = v2

draw_triangle :: Screen ->  (V2 Double, V2 Double, V2 Double) -> Triangle -> IO ()
draw_triangle screen projected_vertices triangle = do
    let bound_min = V2 ((fromIntegral $ toInteger $ width screen) - (1.0 :: Double) ) (((fromIntegral $ toInteger $ height screen) - 1.0 ))
        bound_max = V2 (0 :: Double) (0 :: Double)
        clamped = bound_min
        (v0, v1, v2) = projected_vertices
        bbox_min = V2 bbox_min_x bbox_min_y
        bbox_max = V2 bbox_max_x bbox_max_y
        bbox_min_x =  max 0 (foldr (\(V2 x y) (b) -> (min x  b) ) ((\(V2 xb yb) -> min xb yb) bound_min) [v0, v1, v2])
        bbox_min_y =  max 0 (foldr (\(V2 x y) (b) -> (min y  b) ) ((\(V2 xb yb) -> min xb yb) bound_min) [v0, v1, v2])
        bbox_max_x =  min ((\(V2 xb yb) -> xb) clamped) (foldr (\(V2 x y) (b) -> (max y b) ) ((\(V2 xb yb) -> max xb yb) bound_max) [v0, v1, v2])
        bbox_max_y =  min ((\(V2 xb yb) -> yb) clamped) (foldr (\(V2 x y) (b) -> (max y b) ) ((\(V2 xb yb) -> max xb yb) bound_max) [v0, v1, v2])
    fillpoints <- sequence $ [ sdl_put_pixel screen ( V2 (fromInteger (floor px)) (fromInteger (floor py)) ) (color triangle) | px <- [bbox_min_x .. bbox_max_x], py <- [bbox_min_y .. bbox_max_y], 
                                        let (V3 barx bary barz) = barycentric (projected_vertices) (V2 (realToFrac  px) (realToFrac py)), 
                                                barx >= 0 && bary >=0 && barz >=0 ]
   
    return ()
  
    

-- draw_triangle :: Screen -> V3 Int -> Triangle -> IO ()
-- draw_triangle screen origin triangle = do
--     let (V4 v1a v1b v1c v1d, V4 v2a v2b v2c v2d, V4 v3a v3b v3c v3d) = points triangle
-- sdl_draw_line screen v0 v1 (color triangle)
