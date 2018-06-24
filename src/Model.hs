{-# LANGUAGE OverloadedStrings #-}

module Model
    where

import Prelude
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
import SDL_Aux
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

    diffuse_map <- read_tga "resources/african_head_diffuse.tga"
    
    return $ Model verts'' faces'' norms'' uvs'' diffuse_map (length faces'') (length verts'')



model_face :: Model -> Int -> [Integer]
model_face model ind = [x | face_Vec3 <- ((faces model) V.! ind), let  (x, y, z) = fromVec3 (fst face_Vec3)]

model_vert :: Model -> Int -> Vec.Vec3 Double
model_vert model ind = fst $ (verts model) V.! ind

model_uv :: Model -> Int -> Int -> Vec.Vec2 Int
model_uv model iface nvert = let (x, y, z) = fromVec3 $ fst $ ((faces model) V.! iface) !! nvert
                                 (x', y')  = fromVec2 $ fst $ (uvs model) V.! ( fromIntegral y)
                             in  toVec2  (floor (x' * (fromIntegral $ width $ diffuse_map model)))  (floor (y' * (fromIntegral $ height $ diffuse_map model))) ----- UPDATE THIS

model_diffuse :: Model -> Vec.Vec2 Int -> Vec.Vec4 Word8
model_diffuse model uv = let    (u, v) = fromVec2 uv
                                dm = diffuse_map model
                                image = img dm
                                PixelRGB8 r g b = pixelAt image u v
                         in toVec4 r g b 255

valid_obj_num :: String  -> Bool
valid_obj_num ""  = False
valid_obj_num xs  =
  case dropWhile Char.isDigit xs of
    ""       -> True
    ('.':ys) -> valid_obj_num ys
    ('-':ys) -> valid_obj_num ys
    ('e':'-':ys) -> all Char.isDigit ys
    _        -> False


