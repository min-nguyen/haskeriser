{-# LANGUAGE OverloadedStrings #-}

module Renderer
    where

import Prelude hiding (any, mapM_)
import Control.Monad hiding (mapM_)
import Data.Foldable hiding (elem)
import Data.Maybe
import Data.Word8
import Data.List
import Data.Cross
import Foreign.C.Types
import SDL.Vect
import SDL (($=))
import qualified SDL
import qualified GLM as GLM
import SDL_Aux
import Scene
import Control.Lens

-- # Triangle Vertices v0 v1 v2 -> Queried Point p -> Barycentric Coordinates
barycentric :: (V2 Double, V2 Double, V2 Double) -> V2 Double -> V3 Double
barycentric (v0, v1, v2) p = if abs b2 < 1 then (V3 (-1) 1 1) else V3 (1 - (b0 + b1)/b2) (b1/b2) (b0/b2)
                        where (b0, b1, b2) = cross3(v2x - v0x, v1x - v0x, v0x - px) (v2y - v0y, v1y - v0y, v0y - py)
                              V2 px  py  = p 
                              V2 v0x v0y = v0
                              V2 v1x v1y = v1 
                              V2 v2x v2y = v2

replaceAt :: Double -> Int -> [Double] ->[Double]
replaceAt newElement n array = take n array ++ [newElement] ++ drop (n + 1) array


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
