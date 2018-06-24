{-# LANGUAGE OverloadedStrings #-}

module Shader
    where

import Prelude
import Prelude hiding (any, mapM_)
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
import Data.Vec as Vec hiding (foldr)
import Types
import Util

vertex_shade :: Shader -> Model -> Int -> Int -> (Vec.Vec4 Double, Shader)
vertex_shade shader model iface nthvert =   let gl_vert = (embedVec3to4D $ model_vert model iface nthvert ) :: Vec.Vec4 Double
                                                gl_Vertex = (Vec.multmv (viewport shader) (Vec.multmv (projection shader) (Vec.multmv (modelview shader) gl_vert)))  :: Vec.Vec4 Double
                                                w = (1/(Vec.getElem 2 gl_Vertex)) :: Double
                                                new_col =  multms3 (projectVec4to3D gl_Vertex) w
                                                new_varying_tri = Vec.transpose $ Vec.setElem nthvert new_col (Vec.transpose $ varying_tri shader) 
                                            in (gl_Vertex, shader {varying_tri = new_varying_tri} )

fragment_shade :: Shader -> Model -> Vec3 Double -> Vec4 Word8 -> (Vec4 Word8, Shader)
fragment_shade shader model bary_coords rgba =  let (px, py, pz) = (fromVec3D $ multmv (varying_tri shader) bary_coords) :: (Double, Double, Double)
                                                    color = Vec.map ( (fromIntegral $ floor (pz/200.0)) *) ((toVec4 255 255 255 255) :: Vec4 Word8)
                                                in  (color , shader) 


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



