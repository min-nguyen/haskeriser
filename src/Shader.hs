{-# LANGUAGE OverloadedStrings #-}

module Shader
    where

import Prelude
import Prelude hiding (any, mapM_)
import SDL.Vect
import SDL (($=))
import qualified SDL
import Data.Cross
import Camera
import Matrix
import Model
import SDL_Aux
import Light
import Geometry
import Data.Word8
import qualified Data.Vector as V
import Data.Vec
import Types

-- class IShader a where 
--     vertex :: a -> Rasteriser  -> V3 Double -> V3 Double
--     fragment :: a -> Bool
--     load_shader :: a

-- instance IShader Shader where
--         vertex shader (Rasteriser model screen camera  light ) (V3 a b c)  = 
--             let     projection_mat = cam_projection_matrix camera
--                     viewport_mat = viewport_matrix ((fromIntegral $ width_i screen)/8.0) ((fromIntegral $ height_i screen)/8.0) ((fromIntegral $ width_i screen)*0.75) ((fromIntegral $ height_i screen)*0.75)
--                     eye = V3 1 1 3
--                     center = V3 0 0 0
--                     modelview_mat = lookat_matrix eye center up
--             in (( fromMatV4toV3 ( viewport_mat * projection_mat * modelview_mat * (fromV3toMatV4 (V3 a b c)) )) :: (V3 Double))
--         fragment shader = True
--         load_shader = GouraudShader



