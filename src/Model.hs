{-# LANGUAGE OverloadedStrings #-}


        -- |‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾| -- 
        -- |                                                                        | -- 
        -- |                       MODEL -  OBJ FILE STORAGE                        | -- 
        -- |                                                                        | -- 
        -- |                                                                        | -- 
        --  ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾  -- 


module Model
    where

import Prelude hiding (any, mapM_)
import Control.Monad hiding (mapM_)
import Data.Foldable hiding (elem)
import Data.Maybe
import Data.Word8
import System.Environment
import Data.Char as Char
import Data.List
import Data.Matrix as Matrix
import Data.Cross
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import Foreign.C.Types
import SDL.Vect
import SDL (($=))
import System.IO
import qualified SDL
import Control.Lens
import qualified Data.Vec as Vec
import Data.List.Split
import TGA
import Util
import Codec.Picture
import Codec.Picture.Types
import Camera
import SDLx
import Light
import Types


load_model :: IO Model
load_model = do
    args <- getArgs
    content <- readFile (args !! 0)
    let linesOfFile = lines content
    
        verts = stringListToVec3List $ concat $ map ((filter valid_obj_num) . words) $ filter (\l -> (case l of     (x:y:_) -> x == 'v' && y == ' '
                                                                                                                    _ -> False)) linesOfFile

        norms = stringListToVec3List $ concat $ map ((filter valid_obj_num) . words) $ filter (\l -> (case l of     (x:y:_) -> x == 'v' && y == 'n'
                                                                                                                    _ -> False)) linesOfFile

        uvs   = stringListToVec2List $ map ((filter valid_obj_num) . words) $ filter (\l -> (case l of      (x:y:xs) -> x == 'v' && y == 't'
                                                                                                            _ -> False)) linesOfFile
        
        faces1 =    map2 ((filter valid_obj_num) . (words) . (map (\c -> if (c == '/') then ' ' else c)) ) $ 
                                map words $ filter (\l -> (case l of (x:xs) -> x == 'f'
                                                                     _ -> False)) linesOfFile
                                                                     
        faces2 = stringListToVec3ListI [z | x <- faces1, z <- x, not (null z)]
        faces3 = chunksOf 3 $ map (\n -> (n - 1)) faces2

        nats = 1 : map (+1) (nats)

        faces'' = (V.fromList (zipWith (\f i -> zipWith (\f' i' -> (f', (head i)+i')) f (nats)) faces3 (map (\x -> [x]) ((nats :: [Int])) :: [[Int]]))) :: V.Vector ([(Vec.Vec3 Integer, Int)])
        verts'' = (V.fromList (zipWith (\f i -> (f, i)) verts (nats :: [Int]))) :: V.Vector (Vec.Vec3 Double, Int)
        norms'' = (V.fromList (zipWith (\f i -> (f, i)) norms (nats :: [Int]))) :: V.Vector (Vec.Vec3 Double, Int)
        uvs'' =   (V.fromList (zipWith (\f i -> (f, i)) uvs (nats :: [Int]))) :: V.Vector (Vec.Vec2 Double, Int)
   
    diffuse_map_file <- read_tga "resources/african_head_diffuse.tga"
    normal_map_file  <- read_tga_normal "resources/african_head_nm.tga"

    -- case normal_map_file of 
    --     NormalMap _ _ _ _ _ -> print "normal"
    --     NormalError         -> print "error"
    --     _                   -> print "error?"


    -- print (imgwidth normal_map_file)
    return $ Model verts'' faces'' norms'' uvs'' (length faces'') (length verts'') diffuse_map_file normal_map_file



model_face :: Model -> Int -> [Integer]
model_face model ind = [x | face_Vec3 <- ((faces model) V.! ind), let  (x, y, z) = fromVec3 (fst face_Vec3)]

model_vert :: Model -> Int -> Int -> Vec.Vec3 Double
model_vert model iface nvert  = fst $ (verts model) V.!  ( fromIntegral $ Vec.getElem 0 $ fst ( (faces model V.! iface) !! nvert))

model_uv :: Model -> Int -> Int -> Vec.Vec2 Double
model_uv model iface nvert = let (_, y, _)  = fromVec3 $ fst $ ((faces model) V.! iface) !! nvert
                                 uv         = fst $ (uvs model) V.! ( fromInteger y)
                             in  uv --toVec2  (floor (x' * (fromIntegral $ width $ diffuse_map model)))  (floor (y' * (fromIntegral $ height $ diffuse_map model))) ----- UPDATE THIS

model_diffuse :: Model -> Vec.Vec2 Double -> Vec.Vec4 Word8
model_diffuse model uv = let    (u, v) = fromVec2 uv
                                (u',v') =  (floor (u * (fromIntegral (imgwidth $ diffuse_map model)))  ,  floor (v * (fromIntegral (imgheight $ diffuse_map model)) ))
                                dm = diffuse_map model
                                image = img dm
                                PixelRGB8 r g b = pixelAt image u' v'
                         in toVec4 r g b 255

model_normal :: Model -> Vec.Vec2 Double -> Vec.Vec3 Double                         
model_normal model uv = let (u, v)              =   fromVec2 uv
                            (u', v')            =  (floor (u * (fromIntegral (normwidth $ normal_map model)))  ,  floor (v * (fromIntegral (normheight $ normal_map model)) ))
                            image               = normimg (normal_map model)
                            PixelRGBA8 r g b a  = pixelAt image u' v'
                            color = mapVec4 (fromIntegral) (toVec4 r g b a)
                            rgb = toVec3 (((getElemV4 2 color)/255.0) * 2.0 - 1.0) (((getElemV4 1 color)/255.0) * 2.0 - 1.0) (((getElemV4 0 color)/255.0) * 2.0 - 1.0)
                        in  rgb

valid_obj_num :: String  -> Bool
valid_obj_num ""  = False
valid_obj_num xs  =
  case dropWhile Char.isDigit xs of
    ""       -> True
    ('.':ys) -> valid_obj_num ys
    ('-':ys) -> valid_obj_num ys
    ('e':'-':ys) -> all Char.isDigit ys
    _        -> False


