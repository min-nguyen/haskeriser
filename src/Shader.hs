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
import qualified Data.Vector as V
import Data.Vec as Vec hiding (foldr)
import Camera
import Model
import SDLx
import Light
import Geometry
import Types
import Util




vertex_shade :: Shader -> Rasteriser -> Int -> Int -> (Vec4 Double, Shader)
vertex_shade (CameraShader mview vport proj uni_M uni_MIT uni_Mshadow vary_uv vary_tri) 
            ras iface nthvert = 
                let model = getModel ras
                    vert_uv     = (mapVec2 to_double ((model_uv model iface nthvert ) :: Vec2 Int) ) :: Vec2 Double
                    vert_coords = (embedVec3to4D $ model_vert model iface nthvert ) :: Vec4 Double

                    gl_Vertex = ((multmv (vport)) . (multmv (proj)) . (multmv (mview))) vert_coords  :: Vec4 Double
                    gl_Vertex' = (mult_v3_num (projectVec4to3D gl_Vertex) (1.0/(getElem 2 gl_Vertex))) :: Vec3 Double

                    new_varying_uv  = (setElemV3 nthvert vary_uv vert_uv)
                    new_varying_tri = Vec.transpose $ Vec.setElem nthvert gl_Vertex' (Vec.transpose $ vary_tri)

                in (gl_Vertex, 
                        (CameraShader mview vport proj uni_M uni_MIT uni_Mshadow new_varying_uv new_varying_tri))

vertex_shade (DepthShader modelview viewport projection varying_tri) ras iface nthvert = 
                let model = getModel ras
                    gl_vert = (embedVec3to4D $ model_vert model iface nthvert ) :: Vec4 Double
                    gl_Vertex = ((multmv (viewport)) . (multmv (projection)) . (multmv (modelview))) gl_vert  :: Vec4 Double
                    w = (1.0/(getElem 2 gl_Vertex)) :: Double
                    new_col =  mult_v3_num (projectVec4to3D gl_Vertex) w
                    new_varying_tri = Vec.transpose $ Vec.setElem nthvert new_col (Vec.transpose $ varying_tri) 
                in (gl_Vertex, (DepthShader modelview viewport projection new_varying_tri) )

fragment_shade :: Shader -> Rasteriser -> Vec3 Double -> Vec4 Word8 -> (Vec4 Word8, Shader)
fragment_shade (DepthShader modelview viewport projection varying_tri) ras bary_coords rgba =  
                                                let (px, py, pz) = (fromVec3D $ multmv (varying_tri) bary_coords) :: (Double, Double, Double)
                                                -- |‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾| --                                    
                                                -- |      INSERT RGBA MULTIPLICATION INTO HERE !!                           | -- 
                                                --  ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾  --                                    
                                                    color = mult_rgba_d ((toVec4 255 255 255 255) :: Vec4 Word8) (fromIntegral $ floor (pz/rCONST_depth))
                                                in  (color , (DepthShader modelview viewport projection varying_tri))                         

-- fragment_shade (CameraShader mview vport proj uni_M uni_MIT uni_Mshadow vary_uv vary_tri) model bary_coords rgba =  
--     let sb_p = (multmv uni_Mshadow (embedVec3to4 (multmv vary_tri bary_coords))) :: Vec4 Double   
--         (sb_px, sb_py, sb_pz, sb_pw) = fromVec4 (sb_p/(getElem 3 sb_p))

--         idx =  (floor sb_px) + (floor sb_py) * (screenWidth_i)

--         shadow = 0.3 + 0.7 * ((shadowbuffer V.! idx) < sb_pz)

--         uv = vary_uv * bary_coords
--         n = Vec.normalize $ projectVec4to3 $ multmv uni_MIT (embedVec3to4 (model_normal uv))
--         l = Vec.normalize $ projectVec4to3 $ multmv uni_M (embedVec3to4 light_dir)
--         r = Vec.normalize $ n * (n * l * 2.0) - l) 


--         diff = max 0.0 (n * l)

--         (r,g,b,a) =  fromVec4 (model_diffuse uv)
--         (r',g',b',_) = mapTuple4 (20 +) (r,g,b,a)

--         color = mult_rgba_d ((toVec4 r' g' b' a) :: Vec4 Word8) (fromIntegral $ floor ( shadow * (1.2 * diff + 0.1)  ))

--     in  (color , (CameraShader mview vport proj uni_M uni_MIT uni_Mshadow vary_uv vary_tri))             


load_depthshader :: Shader
load_depthshader =  DepthShader {   modelview       = Vec.identity :: Mat44 Double,
                                    viewport        = Vec.identity :: Mat44 Double,
                                    projection      = Vec.identity :: Mat44 Double,
                                    -- uniform_M       = Vec.identity :: Mat44 Double,
                                    -- uniform_MIT     = Vec.identity :: Mat44 Double,
                                    -- uniform_Mshadow = Vec.identity :: Mat44 Double,
                                    -- varying_uv      = Vec.fromList $ replicate 2 (toVec3Zeros),
                                    varying_tri     = Vec.identity :: Mat33 Double
                                }       

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
