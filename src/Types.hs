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

data Rasteriser = Rasteriser {  
                                getZBuffer     :: V.Vector (Double, Vec4 Word8),
                                getDepthBuffer :: V.Vector (Double, Vec4 Word8),
                                getModel       :: Model,
                                getScreen      :: Screen,
                                getCamera      :: Camera,
                                getLight       :: Light
                            }

data Shader =   DepthShader {
                                modelview   :: Mat44 Double,
                                viewport    :: Mat44 Double,
                                projection  :: Mat44 Double,
                                varying_tri :: Mat33 Double
                            }
                | CameraShader 
                            {
                                modelview       :: Mat44 Double,
                                viewport        :: Mat44 Double,
                                projection      :: Mat44 Double,
                                uniform_M       :: Mat44 Double,
                                uniform_MIT     :: Mat44 Double,
                                uniform_Mshadow :: Mat44 Double,
                                varying_uv      :: Mat32 Double,
                                varying_tri     :: Mat33 Double
                            }

data Model = Model {    
                        verts       :: V.Vector (Vec3 Double, Int),
                        faces       :: V.Vector ([(Vec3 Integer, Int)]),
                        norms       :: V.Vector (Vec3 Double, Int),
                        uvs         :: V.Vector (Vec2 Double, Int),
                        diffuse_map :: TGA_Header,
                        nfaces :: Int,
                        nverts :: Int
                    }
-- |‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾| --                                    
-- |      FINISH INTEGRATING NEW DEPTH SHADER DATA TYPE                     | -- 
--  ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾  --                                  

data Light = Light {direction   :: Vec3 Double}

data Camera = Camera {  position :: Vec4 Double }

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

data TGA_Header = TGA_Header {  
                                width   :: {-# UNPACK #-} !Int,
								height  :: {-# UNPACK #-} !Int,
								imgdata :: ST.Vector (PixelBaseComponent PixelRGB8),
								bbp     :: Int,
								img     :: Image PixelRGB8
                             }
				 | TGA_Error

data Color = Red | Blue | Yellow | Green | White | Purple

data Texture = Texture SDL.Texture (V2 CInt)

---- |‾| ---------------------------------------------------------------------------- |‾| ----
---- | |                                                                              | | ----
---- |_| ---------------------------------------------------------------------------- |_| ----


type ZBuffer = V.Vector (Double, Vec4 Word8)

type ScreenCoords = Vec3 (Vec3 Int) 

rCONST_depth  :: Double
rCONST_depth = 2000.0

screenWidth, screenHeight :: CInt
(screenWidth, screenHeight) = (400, 400)

screenWidth_i, screenHeight_i :: Int
(screenWidth_i, screenHeight_i) = (400, 400)




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