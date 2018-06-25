{-# LANGUAGE OverloadedStrings #-}

        -- |‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾| -- 
        -- |                                                                        | -- 
        -- |                     	     SHADER FUNCTIONS                           | -- 
        -- |                                                                        | -- 
        -- |                                                                        | -- 
        --  ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾  -- 

module Shader
    where

import Prelude hiding (any, mapM_)
import SDL (($=))
import qualified SDL
import Debug.Trace as Trace
import Data.Cross
import Data.Word8
import Data.Vec as Vec hiding (foldr)
import Camera
import Model
import SDLx
import Light
import Geometry
import Types
import Util




-- vertex_shade :: Shader -> Model -> Int -> Int -> (Vec4 Double, Shader)
-- vertex_shade shader model iface nthvert =   let gl_vert = (embedVec3to4D $ model_vert model iface nthvert ) :: Vec4 Double
--                                                 gl_Vertex = ((multmv (viewport shader)) . (multmv (projection shader)) . (multmv (modelview shader))) gl_vert  :: Vec4 Double
--                                                 w = (1.0/(getElem 2 gl_Vertex)) :: Double
--                                                 new_col =  mult_v3_num (projectVec4to3D gl_Vertex) w
--                                                 new_varying_tri = Vec.transpose $ Vec.setElem nthvert new_col (Vec.transpose $ varying_tri shader) 
--                                             in (gl_Vertex, shader {varying_tri = new_varying_tri} )

-- fragment_shade :: Shader -> Model -> Vec3 Double -> Vec4 Word8 -> (Vec4 Word8, Shader)
-- fragment_shade shader model bary_coords rgba =  let (px, py, pz) = (fromVec3D $ multmv (varying_tri shader) bary_coords) :: (Double, Double, Double)
--                                                 -- |‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾| --                                    
--                                                 -- |      INSERT RGBA MULTIPLICATION INTO HERE !!                           | -- 
--                                                 --  ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾  --                                    
--                                                     color = mult_rgba_d ((toVec4 255 255 255 255) :: Vec4 Word8) (fromIntegral $ floor (pz/rCONST_depth))
--                                                 in  debug ((varying_tri shader)) (color , shader) 


-- load_shader :: Shader
-- load_shader = Shader {  modelview       = Vec.identity :: Mat44 Double,
--                         viewport        = Vec.identity :: Mat44 Double,
--                         projection      = Vec.identity :: Mat44 Double,
--                         uniform_M       = Vec.identity :: Mat44 Double,
--                         uniform_MIT     = Vec.identity :: Mat44 Double,
--                         uniform_Mshadow = Vec.identity :: Mat44 Double,
--                         varying_uv      = Vec.fromList $ replicate 2 (toVec3Zeros),
--                         varying_tri     = Vec.identity :: Mat33 Double
--                     }
