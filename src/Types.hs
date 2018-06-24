{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

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
import qualified Data.ByteString as B
import Data.Vector.Storable as ST
import Codec.Picture
import Codec.Picture.Types

data Model = Model {verts       :: V.Vector (Vec3 Double, Int),
                    faces       :: V.Vector ([(Vec3 Integer, Int)]),
                    norms       :: V.Vector (Vec3 Double, Int),
                    uvs         :: V.Vector (Vec2 Double, Int),
                    diffuse_map :: TGA_Header,
                    nfaces :: Int,
                    nverts :: Int}

data Rasteriser = Rasteriser {  model       :: Model,
                                screen      :: Screen,
                                camera      :: Camera,
                                light       :: Light}

data Shader     = Shader    {   modelview       :: Vec4 (Vec4 Double),
                                viewport        :: Vec4 (Vec4 Double),
                                projection      :: Vec4 (Vec4 Double),
                                uniform_M       :: Vec4 (Vec4 Double),
                                uniform_MIT     :: Vec4 (Vec4 Double),
                                uniform_Mshadow :: Vec4 (Vec4 Double),
                                varying_uv      :: Vec2 (Vec3 Double),
                                varying_tri     :: Vec3 (Vec3 Double) }

data TGA_Header = TGA_Header {  width   :: {-# UNPACK #-} !Int,
								height  :: {-# UNPACK #-} !Int,
								imgdata :: ST.Vector (PixelBaseComponent PixelRGB8),
								bbp :: Int,
								img    :: Image PixelRGB8
                            }
				 | TGA_Error


data Light = Light {direction   :: Vec3 Double}

data Camera = Camera {  position :: Vec4 Double }

-- # SDL.Texture, (Width, Height)
data Texture = Texture SDL.Texture (V2 CInt)
-- # Screen

data Screen = Screen  { window    :: SDL.Window,
                        renderer  :: SDL.Renderer,
                        texture   :: Texture,
                        height_c  :: {-# UNPACK #-} !CInt,
                        width_c   :: {-# UNPACK #-} !CInt,
                        height_i  :: {-# UNPACK #-} !Int,
                        width_i   :: {-# UNPACK #-} !Int
                      }


data Triangle = Triangle {  points  :: (Vec4 Double, Vec4 Double, Vec4 Double),
                            color   :: Vec4 Word8 }

data Color = Red | Blue | Yellow | Green | White | Purple

type ZBuffer = V.Vector (Double, Vec4 Word8)

type ScreenCoords = Vec3 (Vec3 Int) 

screenWidth, screenHeight :: CInt
(screenWidth, screenHeight) = (800, 800)
