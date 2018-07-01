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
                                getModelView   :: {-# UNPACK #-} !(Mat44 Double),
                                getViewport    :: {-# UNPACK #-} !(Mat44 Double),
                                getProjection  :: {-# UNPACK #-} !(Mat44 Double),
                                getMVP         :: {-# UNPACK #-} !(Mat44 Double),
                                getCurrentTri  :: {-# UNPACK #-} !(Mat33 Double)
                            }

                | CameraShader 
                            {
                                getModelView      :: {-# UNPACK #-} !(Mat44 Double),
                                getViewport       :: {-# UNPACK #-} !(Mat44 Double),
                                getProjection     :: {-# UNPACK #-} !(Mat44 Double),
                                getMVP            :: {-# UNPACK #-} !(Mat44 Double),
                                getUniformM       :: {-# UNPACK #-} !(Mat44 Double),
                                getUniformMIT     :: {-# UNPACK #-} !(Mat44 Double),
                                getUniformMShadow :: {-# UNPACK #-} !(Mat44 Double),
                                getCurrentUV      :: {-# UNPACK #-} !(Mat23 Double),
                                getCurrentTri     :: {-# UNPACK #-} !(Mat33 Double)
                            }

data Model  =   Model       {    
                                getFace        :: [Face (Mat33 Double) (Mat32 Double) (Mat33 Double) ],
                                getNumFaces :: Int,
                                getNumVerts :: Int,
                                getDiffuseMap :: ColorMap,
                                getNormalMap :: NormalMap,
                                getSpecularMap :: SpecularMap
                            }

data Face       a b c      = Face (a) (b ) (c)
data Vertices    a          = Vertices a a a
data UVs         b          = UVs b b b
data VerticeNormals  c      = VerticeNormals c c c
data Vertex3       a        = Vertex3 a a a
data Vertex2       a        = Vertex2 a a

instance Functor Vertices where
    fmap f (Vertices x y z) = Vertices (f x) (f y) (f z)

instance Applicative Vertices where
    pure a = Vertices a a a 
    Vertices f g h <*> Vertices a b c = Vertices (f a) (g b) (h c)

instance Functor UVs where
    fmap f (UVs x y z) = UVs (f x) (f y) (f z)

instance Applicative UVs where
    pure a = UVs a a a 
    UVs f g h <*> UVs a b c = UVs (f a) (g b) (h c) 

instance Functor VerticeNormals where
    fmap f (VerticeNormals x y z) = VerticeNormals (f x) (f y) (f z)

instance Applicative VerticeNormals where
    pure a = VerticeNormals a a a 
    VerticeNormals f g h <*> VerticeNormals a b c = VerticeNormals (f a) (g b) (h c)

instance Functor Vertex3 where
    fmap f (Vertex3 x y z) = Vertex3 (f x) (f y) (f z)

instance Applicative Vertex3 where
    pure a = Vertex3 a a a 
    Vertex3 f g h <*> Vertex3 a b c = Vertex3 (f a) (g b) (h c)

instance Functor Vertex2 where
    fmap f (Vertex2 x y ) = Vertex2 (f x) (f y) 

instance Applicative Vertex2 where
    pure a = Vertex2 a a 
    Vertex2 f g  <*> Vertex2 a b  = Vertex2 (f a) (g b) 


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


data SpecularMap = SpecularMap {
                                specwidth   :: {-# UNPACK #-} !Int,
                                specheight  :: {-# UNPACK #-} !Int,
                                specdata    :: ST.Vector (PixelBaseComponent Pixel8) ,
                                specbbp     :: Int,
                                specimg     :: Image Pixel8
                            }
                 | SpecularMapError

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
rCONST_depth = 1.0

screenWidth, screenHeight :: CInt
(screenWidth, screenHeight) = (200, 200)

-- debug (bary_coords, sb_pz, currentBufferValue ) 

screenWidth_i, screenHeight_i :: Int
(screenWidth_i, screenHeight_i) = (200, 200)


screenWidth_d, screenHeight_d :: Double
(screenWidth_d, screenHeight_d) = (200, 200)

vec2ToVertex2 :: Vec2 a -> Vertex2 a
vec2ToVertex2 (x:.y:.()) = Vertex2 x y

vec3ToVertex3 :: Vec3 a -> Vertex3 a
vec3ToVertex3 (x:.y:.z:.()) = Vertex3 x y z