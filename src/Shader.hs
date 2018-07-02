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
import System.CPUTime



vertex_shade :: Shader -> Rasteriser -> Face (Mat33 Double) (Mat32 Double) (Mat33 Double)  -> (Mat34 Double, Shader)
vertex_shade (DirectionalLightShader modelview viewport projection mvp_mat varying_tri) ras face = 
    let 
        Face (vertices) (uvs) (vertnorms) = face
        model = getModel ras
   
        screen_vert = (Vec.transpose) $ (multmm :: Mat44 Double -> Mat43 Double -> Mat43 Double) (mvp_mat :: Mat44 Double) (((Vec.transpose :: Mat34 Double -> Mat43 Double) $ Vec.map cartesianToHomogeneous vertices) :: Mat43 Double)   

        new_varying_tri = Vec.transpose $ (Vec.map homogeneousToCartesian) screen_vert

    in ((screen_vert, (DirectionalLightShader modelview viewport projection mvp_mat new_varying_tri) ))
    
vertex_shade (AmbientLightShader modelview viewport projection mvp_mat varying_tri ) ras face = 
    let 
        Face (vertices) (uvs) (vertnorms) = face
        model = getModel ras
   
        screen_vert = (Vec.transpose) $ (multmm :: Mat44 Double -> Mat43 Double -> Mat43 Double) ((multmm projection modelview) :: Mat44 Double) (((Vec.transpose :: Mat34 Double -> Mat43 Double) $ Vec.map cartesianToHomogeneous vertices) :: Mat43 Double)   

        new_varying_tri = Vec.transpose $ (Vec.map homogeneousToCartesian) screen_vert

    in ((screen_vert, (AmbientLightShader modelview viewport projection mvp_mat new_varying_tri ) ))

vertex_shade (CameraShader mview vport proj mvp_mat uni_M uni_MIT uni_Mshadow vary_uv vary_tri) 
        ras face = 
            let model = getModel ras
                Face (vertices) (uvs) (vertnorms) = face
              
                new_vary_uv  = (Vec.transpose uvs) :: Mat23 Double

               

                screen_vert = (Vec.transpose) $ (multmm :: Mat44 Double -> Mat43 Double -> Mat43 Double) (mvp_mat :: Mat44 Double) (((Vec.transpose :: Mat34 Double -> Mat43 Double) $ Vec.map cartesianToHomogeneous vertices) :: Mat43 Double)  
                new_vary_tri = Vec.transpose $  (Vec.map homogeneousToCartesian) screen_vert


            in ((screen_vert, (CameraShader mview vport proj mvp_mat uni_M uni_MIT uni_Mshadow new_vary_uv new_vary_tri) ))



fragment_shade :: Shader -> Rasteriser -> Vec3 Double -> Int -> IO (Rasteriser, Shader)
fragment_shade shader ras bary_coords index = case shader of

    (DirectionalLightShader { getCurrentTri = current_tri, ..}) -> 
       (do
            let 
                (_, _, pz) = (fromVec3D $ multmv current_tri bary_coords) :: (Double, Double, Double)
                updatedbuffer = replaceAt  (pz, toVec4 0 0 0 0) index (getDepthBuffer ras)
                updatedras = ras {getDepthBuffer = updatedbuffer}
            return $ (updatedras , shader) )
    (AmbientLightShader { getCurrentTri = current_tri, ..}) -> 
       (do
            let 
                (px, py, pz)                    = (fromVec3D $ multmv current_tri bary_coords) :: (Double, Double, Double)

                color =  (toVec4 255 255 255 255)
                updatedbuffer = replaceAt  (pz, color) index (getAmbientBuffer ras)
                updatedras = ras {getAmbientBuffer = updatedbuffer}
     
            return $ (updatedras , shader) )
    (CameraShader { getCurrentTri = vary_tri, getCurrentUV = vary_uv, getUniformM = uni_M, getUniformMIT = uni_MIT, getUniformMShadow = uni_Mshadow ,.. }) -> 
       (do
            let shadowBuff = getDepthBuffer ras
                (_, _, pz) = (fromVec3D $ multmv vary_tri bary_coords) :: (Double, Double, Double)
                (sb_px, sb_py, sb_pz)           = fromVec3 $ homogeneousToCartesian (multmv uni_Mshadow (cartesianToHomogeneous (multmv vary_tri bary_coords)))

                currentBufferValue              = fst(shadowBuff V.! (floor ( sb_px + sb_py * (fromIntegral screenWidth_i) )))
               
                isVisible = 0.8 + (sb_pz - pz) * 2.0 

                uv = (multmv ((vary_uv :: Mat23 Double)) (bary_coords :: Vec3 Double)) :: Vec2 Double

                norm    = (Vec.normalize $ homogeneousToCartesian $ multmv uni_MIT (cartesianToHomogeneous (model_normal (getModel ras) uv)))  :: Vec3 Double ----
                light   = (Vec.normalize $ homogeneousToCartesian $ multmv uni_M (cartesianToHomogeneous (direction (getLight ras))) )  :: Vec3 Double
                r       = (Vec.normalize $ (mult_v3_num norm ((dot norm light) * 2.0)) - light ) :: Vec3 Double
                spec    =  (**) (max (getElemV3 2 r) (0.0))  (model_specular (getModel ras) uv)
                diff    = (max 0.0 ((dot norm light) :: Double) ) :: Double

                rgba = (model_diffuse (getModel ras) uv) :: Vec4 Word8

                color =  (add_rgba_d (mult_rgba_d (rgba)   ( isVisible * ( 1.1 * diff + 0.6 * spec ) :: Double)) (10))

            let updatedbuffer = replaceAt  (pz, color) index (getZBuffer ras)
                updatedras = ras {getZBuffer = updatedbuffer}

            return $ (updatedras , shader) )             


load_directionalshader :: Shader
load_directionalshader =  DirectionalLightShader    {   getModelView       = Vec.identity :: Mat44 Double,
                                                        getViewport        = Vec.identity :: Mat44 Double,
                                                        getProjection      = Vec.identity :: Mat44 Double,
                                                        getMVP             = Vec.identity :: Mat44 Double,
                                                        getCurrentTri     = Vec.identity :: Mat33 Double
                                                    }       
load_ambientshader :: Shader
load_ambientshader =  AmbientLightShader    {   getModelView       = Vec.identity :: Mat44 Double,
                                                getViewport        = Vec.identity :: Mat44 Double,
                                                getProjection      = Vec.identity :: Mat44 Double,
                                                getMVP             = Vec.identity :: Mat44 Double,
                                                getCurrentTri      = Vec.identity :: Mat33 Double
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
    DirectionalLightShader _ _ _ _ _ -> (do
        let (Rasteriser zbuffer shadowbuffer ambientbuffer model screen camera light) = rasteriser

            screen_width    = to_double $ width_i screen
            screen_height   = to_double $ height_i screen
            light_dir       = direction light
            depth_coeff     = 0.0

            ----------- SET UP MVP MATRICES IN SHADER -----------

        return $ mvp_shader shader depth_coeff (screen_width/8.0) (screen_height/8.0) (screen_width * (3.0/4.0)) (screen_height * (3.0/4.0)) light_dir center up)
    AmbientLightShader _ _ _ _ _  -> (do
        let (Rasteriser zbuffer shadowbuffer ambientbuffer model screen camera light) = rasteriser
       
       
        let screen_width    = to_double $ width_i screen
            screen_height   = to_double $ height_i screen
            light_dir       = direction light
            depth_coeff     = ((-1.0)/(Vec.norm( eye- center)))
        return $ mvp_shader shader depth_coeff (screen_width/8.0) (screen_height/8.0) (screen_width * (3.0/4.0)) (screen_height * (3.0/4.0)) eye center up)
        
        
    CameraShader _ _ _ _ _ _ _ _ _ -> (do
        let (Rasteriser zbuffer shadowbuffer ambientbuffer model screen camera light) = rasteriser

            screen_width    = to_double $ width_i screen
            screen_height   = to_double $ height_i screen
            light_dir       = direction light
        
            cam_pos         = position camera
            depth_coeff     = ((-1.0)/(Vec.norm( (cam_pos) - center)))

            ----------- SET UP MVP MATRICES IN SHADER -----------
            shade' = mvp_shader shader (depth_coeff) (screen_width/8.0) (screen_height/8.0) (screen_width * (3.0/4.0)) (screen_height * (3.0/4.0)) cam_pos center up

            uniform_M = (getModelView shade')
            
            inv_MV = invert (multmm (getProjection shade') uniform_M)
            uniform_MIT = case inv_MV of Just invProjMod -> (transpose invProjMod :: Mat44 Double)
                                         Nothing         -> (Vec.identity :: Mat44 Double)

            inv_MVP = invert (getMVP shade')
            uniform_MShadow = case inv_MVP of Just invMVP     -> ((multmm previous_mvp invMVP) :: Mat44 Double)
                                              Nothing         -> (Vec.identity :: Mat44 Double)

        return shade' {getUniformM = uniform_M, getUniformMIT = uniform_MIT, getUniformMShadow = uniform_MShadow})