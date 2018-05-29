{-# LANGUAGE OverloadedStrings #-}

module Renderer
    where

import Prelude hiding (any, mapM_)
import Control.Monad hiding (mapM_)
import Data.Foldable hiding (elem)
import Data.Maybe
import Data.Word8
import Foreign.C.Types
import SDL.Vect
import SDL (($=))
import qualified SDL
import qualified GLM as GLM
import SDL_Aux
import Scene


-- draw :: Screen -> V3 Int -> [Triangle] -> IO ()
-- draw screen origin triangles = do
--     return ()


--              # Point Pos             #Screen Pos
-- vertex_shader :: V4 Double -> Camera -> V2 Int
-- vertex_shader point cam =

-- draw_triangle :: Screen -> V3 Int -> Triangle -> IO ()
-- draw_triangle screen origin triangle = do
--     let (V4 v1a v1b v1c v1d, V4 v2a v2b v2c v2d, V4 v3a v3b v3c v3d) = points triangle
--     sdl_draw_line screen v0 v1 (color triangle)