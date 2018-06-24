{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
module TGA where


import Prelude hiding (any, mapM_)
import Control.Monad hiding (mapM_)
import Data.Foldable hiding (elem)
import Data.Maybe
import Foreign.C.Types
import SDL.Vect
import Data.Word8
import Data.Binary.Get
import Data.Word
import SDL (($=))
import qualified SDL
import qualified Data.ByteString as B
import Codec.Picture
import Codec.Picture.Types
import Data.Vector.Storable as V
import Data.Vec
import Types

read_tga :: String -> IO TGA_Header
read_tga filepath = do
	bytestr <- B.readFile filepath
	
	let contents = decodeTga bytestr
	    s = getWord16be

	case contents of 
					Left s  -> return TGA_Error
					Right d -> (case d of
						ImageRGB8  p' -> do
									return $ TGA_Header (imageWidth $   p')  (imageHeight $   p') ((imageData p'):: V.Vector (PixelBaseComponent PixelRGB8))  (8) p'
						
						-- ImageRGBA8 p  -> TGA_Header (imageWidth $   p )  (imageHeight $   p ) (imageData p)  (8) 
						_ -> return TGA_Error)

