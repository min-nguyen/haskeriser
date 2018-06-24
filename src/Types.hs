{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE InstanceSigs #-}
module Types
    where

import Prelude
import Prelude hiding (any, mapM_)
import SDL.Vect
import SDL (($=))
import qualified SDL
import qualified Data.Vector as V
import Data.Vec
import Foreign.C.Types
import Data.Word8
import Data.Binary.Get
import Data.Word
import Control.Applicative 
import Control.Monad (liftM, ap)
import Control.Monad.State
import qualified Data.ByteString as B
import Data.Vector.Storable as ST
import Codec.Picture
import Codec.Picture.Types

---- |‾| ---------------------------------------------------------------------------- |‾| ----
---- | |                       Data type & Instance Definitions                       | | ----
---- |_| ---------------------------------------------------------------------------- |_| ----

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

data Shader  =       Shader { 
                                modelview       ::  Vec4 (Vec4 Double),
                                viewport        ::  Vec4 (Vec4 Double),
                                projection      ::  Vec4 (Vec4 Double),
                                uniform_M       ::  Vec4 (Vec4 Double),
                                uniform_MIT     ::  Vec4 (Vec4 Double),
                                uniform_Mshadow ::  Vec4 (Vec4 Double),
                                varying_uv      ::  Vec2 (Vec3 Double),
                                varying_tri     ::  Vec3 (Vec3 Double)
                               
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

type ZBuffer = V.Vector (Double, Vec4 Word8)

type ScreenCoords = Vec3 (Vec3 Int) 

screenWidth, screenHeight :: CInt
(screenWidth, screenHeight) = (800, 800)



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