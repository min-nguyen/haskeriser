{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE InstanceSigs #-}

        -- |‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾| -- 
        -- |                                                                        | -- 
        -- |                   Data Type & Instance Definitions                     | -- 
        -- |                                                                        | -- 
        -- |                                                                        | -- 
        --  ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾  -- 

module Types
    where

import Prelude hiding (any, mapM_)
import SDL.Vect
import SDL (($=))
import qualified SDL
import qualified Data.Vector as V
import Data.Vec as Vec
import Foreign.C.Types
import Data.Word8
import Data.Binary.Get
import Data.Word
import Control.Applicative 
import Control.Monad (liftM, ap)
import Control.Monad.State
import qualified Data.ByteString as B
import qualified Data.Vector.Storable as ST
import Codec.Picture
import Codec.Picture.Types
import Util




data Model = Model {    
                        verts       :: V.Vector (Vec3 Double, Int),
                        faces       :: V.Vector ([(Vec3 Integer, Int)]),
                        norms       :: V.Vector (Vec3 Double, Int),
                        uvs         :: V.Vector (Vec2 Double, Int),
                        diffuse_map :: TGA_Header,
                        nfaces :: Int,
                        nverts :: Int
                    }

data Rasteriser = Rasteriser {  
                                getModel       :: Model,
                                getScreen      :: Screen,
                                getCamera      :: Camera,
                                getLight       :: Light
                            }

newtype Kernel s a = Kernel  {  
                                runKernel       ::   s -> (a, s)
                             }


data Shader =   DepthShader {
                                modelview   :: Mat44 Double,
                                viewport    :: Mat44 Double,
                                projection  :: Mat44 Double,
                                varying_tri :: Mat33 Double
                            }


-- |‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾| --                                    
-- |      FINISH INTEGRATING NEW DEPTH SHADER DATA TYPE                     | -- 
--  ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾  --                                  

vertex_shade :: Shader -> Model -> Int -> Int -> (Vec4 Double, Shader)
vertex_shade (DepthShader modelview viewport projection varying_tri) = 
                    \model iface nthvert ->     let shader = DepthShader
                                                    gl_vert = (embedVec3to4D $ model_vert model iface nthvert ) :: Vec4 Double
                                                    gl_Vertex = ((multmv (viewport shader)) . (multmv (projection shader)) . (multmv (modelview shader))) gl_vert  :: Vec4 Double
                                                    w = (1.0/(getElem 2 gl_Vertex)) :: Double
                                                    new_col =  mult_v3_num (projectVec4to3D gl_Vertex) w
                                                    new_varying_tri = Vec.transpose $ Vec.setElem nthvert new_col (Vec.transpose $ varying_tri shader) 
                                                in (gl_Vertex, shader {varying_tri = new_varying_tri} )

fragment_shade :: Shader -> Model -> Vec3 Double -> Vec4 Word8 -> (Vec4 Word8, Shader)
fragment_shade shader model bary_coords rgba =  let (px, py, pz) = (fromVec3D $ multmv (varying_tri shader) bary_coords) :: (Double, Double, Double)
                                                -- |‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾| --                                    
                                                -- |      INSERT RGBA MULTIPLICATION INTO HERE !!                           | -- 
                                                --  ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾  --                                    
                                                    color = mult_rgba_d ((toVec4 255 255 255 255) :: Vec4 Word8) (fromIntegral $ floor (pz/rCONST_depth))
                                                in  debug ((varying_tri shader)) (color , shader)                         
                                                
                                                
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

data Light = Light {direction   :: Vec3 Double}

data Camera = Camera {  position :: Vec4 Double }

                            -- (Width, Height) --
data Texture = Texture SDL.Texture (V2 CInt)

data Screen = Screen  { 
                        window    :: SDL.Window,
                        renderer  :: SDL.Renderer,
                        texture   :: Texture,
                        height_c  :: {-# UNPACK #-} !CInt,
                        width_c   :: {-# UNPACK #-} !CInt,
                        height_i  :: {-# UNPACK #-} !Int,
                        width_i   :: {-# UNPACK #-} !Int
                      }



data TGA_Header = TGA_Header {  
                                width   :: {-# UNPACK #-} !Int,
								height  :: {-# UNPACK #-} !Int,
								imgdata :: ST.Vector (PixelBaseComponent PixelRGB8),
								bbp     :: Int,
								img     :: Image PixelRGB8
                             }
				 | TGA_Error

data Triangle = Triangle {  
                            points  :: (Vec4 Double, Vec4 Double, Vec4 Double),
                            color   :: Vec4 Word8 
                         }

data Color = Red | Blue | Yellow | Green | White | Purple


---- |‾| ---------------------------------------------------------------------------- |‾| ----
---- | |                                                                              | | ----
---- |_| ---------------------------------------------------------------------------- |_| ----


type ZBuffer = V.Vector (Double, Vec4 Word8)

type ScreenCoords = Vec3 (Vec3 Int) 

rCONST_depth  :: Double
rCONST_depth = 2000.0

screenWidth, screenHeight :: CInt
(screenWidth, screenHeight) = (150, 150)

get  :: Kernel s s
get  = Kernel {runKernel = (\s -> (s, s)) }

set  :: s -> Kernel s ()
set s = Kernel {runKernel = (\s' -> ((), s) ) }

instance Monad (Kernel s) where
    (>>=) :: Kernel s a -> (a -> Kernel s b) -> Kernel s b
    f >>=  g = Kernel {runKernel = (\s -> (let (a', s')      = (runKernel f) s
                                           in (runKernel (g a')) s'))
                                          }
    return :: a -> Kernel s a
    return x = Kernel  {runKernel = (\shader_st -> (x, shader_st))}


instance Functor (Kernel s) where 
    fmap = liftM

instance Applicative (Kernel s) where
    pure = return
    (<*>) = ap