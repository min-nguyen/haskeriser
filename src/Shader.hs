{-# LANGUAGE OverloadedStrings #-}

module Shader
    where

import Prelude
import Prelude hiding (any, mapM_)
import SDL (($=))
import Debug.Trace as Trace
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
                                            in Trace.trace (show gl_Vertex) (gl_Vertex, shader {varying_tri = new_varying_tri} )

fragment_shade :: Shader -> Model -> Vec3 Double -> Vec4 Word8 -> (Vec4 Word8, Shader)
fragment_shade shader model bary_coords rgba =  let (px, py, pz) = (fromVec3D $ multmv (varying_tri shader) bary_coords) :: (Double, Double, Double)
                                                    color = Vec.map ( (fromIntegral $ floor (pz/200.0)) *) ((toVec4 255 255 255 255) :: Vec4 Word8)
                                                in  (color , shader) 


load_shader :: Shader
load_shader = Shader {  modelview       = toVec4 (toVec4 0 0 0 0) (toVec4 0 0 0 0) (toVec4 0 0 0 0) (toVec4 0 0 0 0),
                        viewport        = toVec4 (toVec4 0 0 0 0) (toVec4 0 0 0 0) (toVec4 0 0 0 0) (toVec4 0 0 0 0),
                        projection      = toVec4 (toVec4 0 0 0 0) (toVec4 0 0 0 0) (toVec4 0 0 0 0) (toVec4 0 0 0 0),
                        uniform_M       = toVec4 (toVec4 0 0 0 0) (toVec4 0 0 0 0) (toVec4 0 0 0 0) (toVec4 0 0 0 0),
                        uniform_MIT     = toVec4 (toVec4 0 0 0 0) (toVec4 0 0 0 0) (toVec4 0 0 0 0) (toVec4 0 0 0 0),
                        uniform_Mshadow = toVec4 (toVec4 0 0 0 0) (toVec4 0 0 0 0) (toVec4 0 0 0 0) (toVec4 0 0 0 0),
                        varying_uv      = toVec2 (toVec3 0 0 0) (toVec3 0 0 0),
                        varying_tri     = toVec3 (toVec3 0 0 0) (toVec3 0 0 0) (toVec3 0 0 0)
                    }




