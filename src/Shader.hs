{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

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
vertex_shade (DepthShader modelview viewport projection mvp_mat varying_tri) ras iface nthvert = 
    let model = getModel ras
        gl_vert = (cartesianToHomogeneous $ model_vert model iface nthvert ) :: Vec4 Double
        gl_Vertex = multmv mvp_mat gl_vert  :: Vec4 Double

        new_col =  (homogeneousToCartesian gl_Vertex) 
        new_varying_tri = Vec.transpose $ Vec.setElem nthvert new_col (Vec.transpose $ varying_tri)

    in (gl_Vertex, (DepthShader modelview viewport projection mvp_mat new_varying_tri) )

vertex_shade (CameraShader mview vport proj mvp_mat uni_M uni_MIT uni_Mshadow vary_uv vary_tri) 
        ras iface nthvert = 
            let model = getModel ras
                vert_uv     = ((model_uv model iface nthvert ) :: Vec2 Double)
                new_vary_uv  = (Vec.transpose (setElemV3 nthvert (Vec.transpose vary_uv) vert_uv)) :: Mat23 Double

                vert_coords = (cartesianToHomogeneous $ model_vert model iface nthvert ) :: Vec4 Double

                gl_Vertex =  multmv mvp_mat vert_coords  :: Vec4 Double
                gl_Vertex' = (homogeneousToCartesian gl_Vertex ) :: Vec3 Double

                
                new_vary_tri = Vec.transpose $ Vec.setElem nthvert gl_Vertex' (Vec.transpose $ vary_tri)

            in (gl_Vertex, (CameraShader mview vport proj mvp_mat uni_M uni_MIT uni_Mshadow new_vary_uv new_vary_tri) )



fragment_shade :: Shader -> Rasteriser -> Vec3 Double -> (Vec4 Word8, Shader)
fragment_shade shader ras bary_coords  = case shader of

    (DepthShader { getCurrentTri = current_tri, ..}) -> 
       (let (px, py, pz) = (fromVec3D $ multmv current_tri bary_coords) :: (Double, Double, Double)    
            color = mult_rgba_d ((toVec4 255 255 255 255) :: Vec4 Word8) (pz/rCONST_depth)
        in  (color , shader)  )
    
    (CameraShader { getCurrentTri = vary_tri, getCurrentUV = vary_uv, getUniformM = uni_M, getUniformMIT = uni_MIT, getUniformMShadow = uni_Mshadow ,.. }) -> 
       (let shadowBuff = getDepthBuffer ras

            sb_p                            = (multmv uni_Mshadow (cartesianToHomogeneous (multmv vary_tri bary_coords))) :: Vec4 Double   
            (sb_px, sb_py, sb_pz, sb_pw)    = fromVec4 sb_p

            currentBufferValue              = fst(shadowBuff V.! (floor ( sb_px + sb_py * (fromIntegral screenWidth_i) )))

            isVisible = if (currentBufferValue < sb_pz) then sb_pz else currentBufferValue

            shadow = (4000.0/((isVisible/125.0)*(isVisible/125.0))) :: Double

            uv = (multmv ((vary_uv :: Mat23 Double)) (bary_coords :: Vec3 Double)) :: Vec2 Double
            n  = (Vec.normalize $ homogeneousToCartesian $ multmv uni_MIT (cartesianToHomogeneous (model_normal (getModel ras) uv)))  :: Vec3 Double ----
            l  = (Vec.normalize $ homogeneousToCartesian $ multmv uni_M (cartesianToHomogeneous (Vec.normalize $ direction (getLight ras))) )  :: Vec3 Double
            -- r  = (Vec.normalize $ (mult_v3_num n (mult_v3_num (multvv n l) 2.0)) - l ) :: Vec3 Double 


            diff = (max 0.0 ((dot n l) :: Double) ) :: Double

            rgba = (model_diffuse (getModel ras) uv) :: Vec4 Word8

            color = add_rgba_d (mult_rgba_d rgba  ( ( shadow * ( 1.4 * diff + 0.6)  ) :: Double) ) 20.0

        in  (color , shader) )             


load_depthshader :: Shader
load_depthshader =  DepthShader {   getModelView       = Vec.identity :: Mat44 Double,
                                    getViewport        = Vec.identity :: Mat44 Double,
                                    getProjection      = Vec.identity :: Mat44 Double,
                                    getMVP             = Vec.identity :: Mat44 Double,
                                    getCurrentTri     = Vec.identity :: Mat33 Double
                                }       

load_camerashader :: Shader
load_camerashader =  CameraShader {     getModelView       = Vec.identity :: Mat44 Double,
                                        getViewport        = Vec.identity :: Mat44 Double,
                                        getProjection      = Vec.identity :: Mat44 Double,
                                        getMVP             = Vec.identity :: Mat44 Double,
                                        getUniformM       = Vec.identity :: Mat44 Double,
                                        getUniformMIT     = Vec.identity :: Mat44 Double,
                                        getUniformMShadow = Vec.identity :: Mat44 Double,
                                        getCurrentUV      = Vec.fromList $ replicate 2 (toVec3Zeros),
                                        getCurrentTri     = Vec.identity :: Mat33 Double
                                  }       


setup_shader :: Rasteriser -> Shader -> Mat44 Double -> IO Shader
setup_shader rasteriser shader previous_mvp = case shader of
    DepthShader _ _ _ _ _ -> (do
        let (Rasteriser zbuffer shadowbuffer model screen camera light) = rasteriser

            screen_width    = to_double $ width_i screen
            screen_height   = to_double $ height_i screen
            light_dir       = Vec.normalize $ direction light
            depth_coeff     = 0.0

            ----------- SET UP MVP MATRICES IN SHADER -----------

        return $ mvp_shader shader depth_coeff (screen_width/8.0) (screen_height/8.0) (screen_width * (3.0/4.0)) (screen_height * (3.0/4.0)) light_dir center up)
        
    CameraShader _ _ _ _ _ _ _ _ _ -> (do
        let (Rasteriser zbuffer shadowbuffer model screen camera light) = rasteriser

            screen_width    = to_double $ width_i screen
            screen_height   = to_double $ height_i screen
            light_dir       = Vec.normalize $ direction light
        
            cam_pos         = position camera
            depth_coeff     = ((-1.0)/(Vec.norm(eye-center)))

            ----------- SET UP MVP MATRICES IN SHADER -----------
            shade' = (mvp_shader shader (depth_coeff) (screen_width/8.0) (screen_height/8.0) (screen_width * (3.0/4.0)) (screen_height * (3.0/4.0)) eye center up)

            uniform_M = (getModelView shade')
            
            inv_MV = invert (multmm (getProjection shade') uniform_M)
            uniform_MIT = case inv_MV of Just invProjMod -> (transpose invProjMod :: Mat44 Double)
                                         Nothing         -> (Vec.identity :: Mat44 Double)

            inv_MVP = invert (getMVP shade')
            uniform_MShadow = case inv_MVP of Just invMVP     -> ((multmm previous_mvp invMVP) :: Mat44 Double)
                                              Nothing         -> (Vec.identity :: Mat44 Double)

        return shade' {getUniformM = uniform_M, getUniformMIT = uniform_MIT, getUniformMShadow = uniform_MShadow})