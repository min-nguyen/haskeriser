{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module TGA where


import Prelude hiding (any, mapM_)
import Control.Monad hiding (mapM_)
import Data.Foldable hiding (elem)
import Data.Maybe
import Foreign.C.Types
import SDL.Vect
import Data.Word8
import SDL (($=))
import qualified SDL
import qualified Data.ByteString as B
import Codec.Picture
import Codec.Picture.Types

data TGA_Header = TGA_Header {  idlength :: Char,
                                colormaptype :: Char,
                                datatypecode :: Char,
                                colormaporigin :: CShort,
                                colormaplength :: CShort,
                                colormapdepth :: Char,
                                x_origin :: CShort,
                                y_origin :: CShort,
                                width :: CShort,
                                height :: CShort,
                                bitsperpixel :: Char,
                                imagedescriptor :: Char
                            }
read_tga :: String -> IO ()
read_tga filepath = do
    bytestr <- B.readFile filepath
    let contents = decodeTga bytestr
        image = case contents of Left s -> print s
                                 Right d -> case d of 
                                    ImageRGBA8 p -> print "yay"
                                    ImageRGB8  p' -> print "yay'"
                                    _ -> print "oh"
    return ()
	-- in.read((char *)&header, sizeof(header));
	-- if (!in.good()) {
	-- 	in.close();
	-- 	std::cerr << "an error occured while reading the header\n";
	-- 	return false;
	-- }
	-- width   = header.width;
	-- height  = header.height;
	-- bytespp = header.bitsperpixel>>3;
	-- if (width<=0 || height<=0 || (bytespp!=GRAYSCALE && bytespp!=RGB && bytespp!=RGBA)) {
	-- 	in.close();
	-- 	std::cerr << "bad bpp (or width/height) value\n";
	-- 	return false;
	-- }
	-- unsigned long nbytes = bytespp*width*height;
	-- data = new unsigned char[nbytes];
	-- if (3==header.datatypecode || 2==header.datatypecode) {
	-- 	in.read((char *)data, nbytes);
	-- 	if (!in.good()) {
	-- 		in.close();
	-- 		std::cerr << "an error occured while reading the data\n";
	-- 		return false;
	-- 	}
	-- } else if (10==header.datatypecode||11==header.datatypecode) {
	-- 	if (!load_rle_data(in)) {
	-- 		in.close();
	-- 		std::cerr << "an error occured while reading the data\n";
	-- 		return false;
	-- 	}
	-- } else {
	-- 	in.close();
	-- 	std::cerr << "unknown file format " << (int)header.datatypecode << "\n";
	-- 	return false;
	-- }
	-- if (!(header.imagedescriptor & 0x20)) {
	-- 	flip_vertically();
	-- }
	-- if (header.imagedescriptor & 0x10) {
	-- 	flip_horizontally();
	-- }
	-- std::cerr << width << "x" << height << "/" << bytespp*8 << "\n";
	-- in.close();
	-- return true;