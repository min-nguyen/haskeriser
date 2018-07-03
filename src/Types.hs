{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
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
import Control.Monad.Par as Par
import Control.Monad.State
import qualified Data.ByteString as B
import qualified Data.Vector.Storable as ST
import Codec.Picture
import Codec.Picture.Types
import Util
import Control.DeepSeq
-- import Control.DeepSeq.Generics (genericRnf)
import GHC.Generics

data Rasteriser = Rasteriser 
                            {  
                                getZBuffer     ::  {-# UNPACK #-} !(V.Vector (Double, Vec4 Word8)),
                                getDepthBuffer ::  {-# UNPACK #-} !(V.Vector (Double, Vec4 Word8)),
                                getAmbientBuffer ::  {-# UNPACK #-} !(V.Vector (Double, Vec4 Word8)),
                                getModel       ::  {-# UNPACK #-} !Model,
                                getScreen      ::  {-# UNPACK #-} !Screen,
                                getCamera      ::  {-# UNPACK #-} !Camera,
                                getLight       ::  {-# UNPACK #-} !Light
                            } 
                         

instance NFData Rasteriser
    where rnf x = seq x () 

data Shader =   DirectionalLightShader 
                            {
                                getModelView   :: {-# UNPACK #-} !(Mat44 Double),
                                getViewport    :: {-# UNPACK #-} !(Mat44 Double),
                                getProjection  :: {-# UNPACK #-} !(Mat44 Double),
                                getMVP         :: {-# UNPACK #-} !(Mat44 Double),
                                getTransformM  :: {-# UNPACK #-} !(Mat44 Double),
                                getCurrentTri  :: {-# UNPACK #-} !(Mat33 Double)
                            }
                | AmbientLightShader 
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

instance NFData Shader
    where rnf x = seq x () 

data Model  =   Model       {    
                                getFace         :: [Face],
                                getNumFaces     ::  {-# UNPACK #-} !Int,
                                getNumVerts     ::  {-# UNPACK #-} !Int,
                                getDiffuseMap   :: ColorMap,
                                getNormalMap    :: NormalMap,
                                getSpecularMap  :: SpecularMap
                            }

data Face               = Face {
                                vertices ::  {-# UNPACK #-} !(Mat33 Double),
                                uvs      ::  {-# UNPACK #-} !(Mat23 Double),
                                vertnorms :: {-# UNPACK #-} !(Mat33 Double)
                             }
data Vertices    a         = Vertices a a a

instance Functor Vertices where
    fmap f (Vertices x y z) = Vertices (f x) (f y) (f z)

instance Applicative Vertices where
    pure a = Vertices a a a 
    Vertices f g h <*> Vertices a b c = Vertices (f a) (g b) (h c)

data Light  = Light {
                        direction   :: Vec3 Double,
                        intensity   :: Double
                    }

data Camera = Camera {  
                        position :: Vec3 Double,
                        rotation :: Vec3 Double
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

data ColorMap = ColorMap  {  
                        imgwidth   :: {-# UNPACK #-} !Int,
                        imgheight  :: {-# UNPACK #-} !Int,
                        imgdata    :: {-# UNPACK #-} !(ST.Vector (PixelBaseComponent PixelRGB8)),
                        imgbbp     :: Int,
                        img        :: Image PixelRGB8
                }
                | ColorMapError

data NormalMap = NormalMap {
                                normwidth   :: {-# UNPACK #-} !Int,
                                normheight  :: {-# UNPACK #-} !Int,
                                normdata    :: {-# UNPACK #-} !(ST.Vector (PixelBaseComponent PixelRGB8)) ,
                                normbbp     :: Int,
                                normimg     :: Image PixelRGB8
                            }
                 | NormalMapError


data SpecularMap = SpecularMap {
                                specwidth   :: {-# UNPACK #-} !Int,
                                specheight  :: {-# UNPACK #-} !Int,
                                specdata    :: {-# UNPACK #-} !(ST.Vector (PixelBaseComponent PixelRGB8)) ,
                                specbbp     :: Int,
                                specimg     :: Image PixelRGB8
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

type Buffer = V.Vector (Double, Vec4 Word8)

type ScreenCoords = Vec3 (Vec3 Int) 

rCONST_depth  :: Double
rCONST_depth = 255.0

m_PI  :: Double
m_PI = 3.1416

screenWidth, screenHeight :: CInt
(screenWidth, screenHeight) = (800, 800)

screenWidth_i, screenHeight_i :: Int
(screenWidth_i, screenHeight_i) = (800, 800)


screenWidth_d, screenHeight_d :: Double
(screenWidth_d, screenHeight_d) = (800, 800)

