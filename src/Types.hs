{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RecordWildCards #-}

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

data Rasteriser = Rasteriser 
                            {  
                                getZBuffer     :: V.Vector (Double, Vec4 Word8),
                                getDepthBuffer :: V.Vector (Double, Vec4 Word8),
                                getModel       :: Model,
                                getScreen      :: Screen,
                                getCamera      :: Camera,
                                getLight       :: Light
                            }

data Shader =   DepthShader 
                            {
                                getModelView   :: Mat44 Double,
                                getViewport    :: Mat44 Double,
                                getProjection  :: Mat44 Double,
                                getMVP         :: Mat44 Double,
                                getCurrentTri  :: Mat33 Double
                            }

                | CameraShader 
                            {
                                getModelView      :: Mat44 Double,
                                getViewport       :: Mat44 Double,
                                getProjection     :: Mat44 Double,
                                getMVP            :: Mat44 Double,
                                getUniformM       :: Mat44 Double,
                                getUniformMIT     :: Mat44 Double,
                                getUniformMShadow :: Mat44 Double,
                                getCurrentUV      :: Mat32 Double,
                                getCurrentTri     :: Mat33 Double
                            }

data Model  =   Model       {    
                                getVerts       :: V.Vector (Vec3 Double, Int),
                                getFaces       :: V.Vector ([(Vec3 Integer, Int)]),
                                getNorms       :: V.Vector (Vec3 Double, Int),
                                getUVs         :: V.Vector (Vec2 Double, Int),
                                getNumFaces :: Int,
                                getNumVerts :: Int,
                                getDiffuseMap :: ColorMap,
                                getNormalMap :: NormalMap
                            }

data Light  = Light {
                        direction   :: Vec3 Double
                    }

data Camera = Camera {  
                        position :: Vec3 Double 
                     }

data Screen = Screen  { 
                        window    :: SDL.Window,
                        renderer  :: SDL.Renderer,
                        texture   :: Texture,
                        height_c  :: {-# UNPACK #-} !CInt,
                        width_c   :: {-# UNPACK #-} !CInt,
                        height_i  :: {-# UNPACK #-} !Int,
                        width_i   :: {-# UNPACK #-} !Int
                      }
                      
data Triangle = Triangle {  
                            points  :: (Vec4 Double, Vec4 Double, Vec4 Double),
                            color   :: Vec4 Word8 
                         }

data ColorMap =  ColorMap  {  
                                    imgwidth   :: {-# UNPACK #-} !Int,
                                    imgheight  :: {-# UNPACK #-} !Int,
                                    imgdata    :: ST.Vector (PixelBaseComponent PixelRGB8),
                                    imgbbp     :: Int,
                                    img        :: Image PixelRGB8
                                }
				 | ColorMapError

data NormalMap = NormalMap {
                                normwidth   :: {-# UNPACK #-} !Int,
                                normheight  :: {-# UNPACK #-} !Int,
                                normdata    :: ST.Vector (PixelBaseComponent PixelRGBA8) ,
                                normbbp     :: Int,
                                normimg     :: Image PixelRGBA8
                            }
                 | NormalMapError

data Color = Red | Blue | Yellow | Green | White | Purple

data Texture = Texture SDL.Texture (V2 CInt)

newtype Kernel s a = Kernel  {  
                                runKernel       ::   s -> (a, s)
                             }

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




    ---- |‾| -------------------------------------------------------------- |‾| ----
     --- | |                        Type Aliases                            | | ---
      --- ‾------------------------------------------------------------------‾---

type ZBuffer = V.Vector (Double, Vec4 Word8)

type ScreenCoords = Vec3 (Vec3 Int) 

rCONST_depth  :: Double
rCONST_depth = 20.0

screenWidth, screenHeight :: CInt
(screenWidth, screenHeight) = (200, 200)

-- debug (bary_coords, sb_pz, currentBufferValue ) 

screenWidth_i, screenHeight_i :: Int
(screenWidth_i, screenHeight_i) = (200, 200)



    