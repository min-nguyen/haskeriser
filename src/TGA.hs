{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}

		-- |‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾| -- 
		-- |                                                                        | -- 
		-- |                     	   TGA FILE HANDLING                            | -- 
		-- |                                                                        | -- 
		-- |                                                                        | -- 
		--  ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾  -- 

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
import Util
import GHC.Generics (Generic, Generic1)
import Control.DeepSeq

read_tga_color :: String -> IO ColorMap
read_tga_color filepath = do
	bytestr <- B.readFile filepath
	
	let contents = decodeTga bytestr
	

	return $ case contents of 
					Left s  -> debug  "HELLO" ColorMapError
					Right d -> (case convertRGB8 d of
						Image w h d ->  (ColorMap w h d (8) (Image w h d ))
						-- ImageY16 c' -> (do
						-- 			putStrLn "ye"
						-- 			return (TGA_NormalMap (imageWidth $   c')  (imageHeight $  c') ((imageData c') :: V.Vector (Word8))  (8) c'))
						-- ImageRGBA8 p  -> TGA_Header (imageWidth $   p )  (imageHeight $   p ) (imageData p)  (8) 
						_ -> ( ( ColorMapError)))

read_tga_specular :: String -> IO (SpecularMap)
read_tga_specular filepath = do
	bytestr <- B.readFile filepath
	
	let contents = decodeTga bytestr
	

	case contents of 
			Left s  -> return SpecularMapError
			Right k -> (case convertRGB8 k of
				
				Image w h d ->  return (SpecularMap w h d (8) (Image w h d ))
				-- ImageY16 c' -> (do
				-- 			putStrLn "ye"
				-- 			return (TGA_NormalMap (imageWidth $   c')  (imageHeight $  c') ((imageData c') :: V.Vector (Word8))  (8) c'))
				-- ImageRGBA8 p  -> TGA_Header (imageWidth $   p )  (imageHeight $   p ) (imageData p)  (8) 
				_ ->  return SpecularMapError)


read_tga_normal :: String -> IO NormalMap
read_tga_normal filepath = do
	bytestr <- B.readFile filepath
	
	let contents = decodeTga bytestr
	

	return $ case contents of 
					Left s  -> debug "HELLO" NormalMapError
					Right k -> (case convertRGB8 k of
						Image w h d ->  (NormalMap w h d (8) (Image w h d ))
						_ -> ( NormalMapError))				



print_tga_type :: String -> IO ()
print_tga_type filepath = do
	bytestr <- B.readFile filepath
	
	let contents = decodeTga bytestr

	case contents of 
					Left  s  -> print s
					Right r -> (case r of
						ImageY16  a  	-> print "ImageY16" 
						ImageY8   b  	-> print "ImageY8"  
						ImageYF   c  	-> print "ImageYF"  
						ImageYA8  d  	-> print "ImageYA8"
						ImageYA16 e  	-> print "ImageYA16"
						ImageRGB8 f  	-> print "ImageRGB8"
						ImageRGB16 g 	-> print "ImageRGB16"
						ImageRGBF h  	-> print "ImageRGBF"
						ImageRGBA8 i  	-> print "ImageRGBA8"
						ImageRGBA16 j 	-> print "ImageRGBA16"
						_  -> print "No Pattern Match")

