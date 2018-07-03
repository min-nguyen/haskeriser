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


    spec_map_file <- read_tga_specular "resources/african_head_spec.tga"
    diffuse_map_file <- read_tga_color "resources/african_head_diffuse.tga"
    normal_map_file  <- read_tga_normal "resources/african_head_nm.tga"

    let facelist = load_faces (V.fromList verts) (V.fromList uvs) (V.fromList norms) faces''
        model = Model facelist (length faces'') (length verts'') diffuse_map_file normal_map_file spec_map_file
    -- print $ pixelAt  (specimg $ getSpecularMap model) 5 5 
    return model

load_faces :: V.Vector (Vec.Vec3 Double) -> V.Vector (Vec.Vec2 Double) -> V.Vector (Vec.Vec3 Double) -> V.Vector ([(Vec.Vec3 Integer, Int)]) -> [Face ]
load_faces verts uvs vertnorms obj_faces = [ Face (face_verts  verts i) (face_uvs  uvs i) (face_vertnorms  vertnorms i)  | i <- [0 .. (length obj_faces - 1)]]
    where   face_vertnorms   vertnorms iface =  (toVec3 (model_vertnorms  vertnorms obj_faces iface 0)  (model_vertnorms  vertnorms obj_faces iface 1) (model_vertnorms  vertnorms obj_faces iface 2)) :: Vec.Mat33 Double
            face_verts   verts iface =  (toVec3 (model_vert  verts obj_faces iface 0)  (model_vert  verts  obj_faces iface 1) (model_vert  verts obj_faces iface 2)) :: Vec.Mat33 Double
            face_uvs    uvs iface =  (Vec.transpose $ toVec3 (model_uv  uvs obj_faces iface 0)  (model_uv  uvs obj_faces iface 1) (model_uv  uvs obj_faces iface 2)) :: Vec.Mat23 Double

model_vert :: V.Vector (Vec.Vec3 Double) -> V.Vector ([(Vec.Vec3 Integer, Int)]) -> Int -> Int -> Vec.Vec3 Double
model_vert verts obj_faces iface nvert  =  ( (verts V.! (fromIntegral ( (Vec.getElem 0 (fst ((obj_faces V.! iface) !! nvert))) )  )  ))

model_uv :: V.Vector (Vec.Vec2 Double) -> V.Vector ([(Vec.Vec3 Integer, Int)]) -> Int -> Int -> Vec.Vec2 Double
model_uv uvs obj_faces iface nvert =                        ( let   y          =  fromIntegral ( (Vec.getElem 1 (fst (( obj_faces V.! iface) !! nvert))) )  
                                                                    uv         =  (uvs V.! y)
                                                              in    uv) --toVec2  (floor (x' * (fromIntegral $ width $ diffuse_map model)))  (floor (y' * (fromIntegral $ height $ diffuse_map model))) ----- UPDATE THIS

model_vertnorms ::  V.Vector (Vec.Vec3 Double) -> V.Vector ([(Vec.Vec3 Integer, Int)]) -> Int -> Int -> Vec.Vec3 Double
model_vertnorms vertnorms obj_faces iface nvert =       ( let z          = fromIntegral ( (Vec.getElem 2 (fst (( obj_faces V.! iface) !! nvert))) )  
                                                              vertnorm   =  ((vertnorms) V.! z)
                                                          in  vertnorm)

model_diffuse :: Model -> Vec.Vec2 Double -> Vec.Vec4 Word8
model_diffuse model uv = let    (u, v) = fromVec2 uv
                                (u',v') =  (floor ( u * (fromIntegral (imgwidth $ getDiffuseMap model)) ) ,  floor ( (fromIntegral $ imgheight $ getDiffuseMap model) - v * (fromIntegral (imgheight $ getDiffuseMap model)) ) )
                                dm =  (getDiffuseMap model)
                                image = img dm
                                PixelRGB8 r g b = pixelAt image u' v'
                         in toVec4 r g b 255

model_normal :: Model -> Vec.Vec2 Double -> Vec.Vec3 Double                         
model_normal model uv = let (u, v)              =   fromVec2 uv
                            (u', v')            =  (floor (u * (fromIntegral (normwidth $ getNormalMap model)))  ,  floor ( fromIntegral (normheight $ getNormalMap model) - v * (fromIntegral (normheight $ getNormalMap model)) ))
                            image               =   (normimg (getNormalMap model))
                            PixelRGBA8 r g b a  = pixelAt image u' v'
                            color = Vec.map (fromIntegral) (toVec4 r g b a)
                            rgb = toVec3 (((getElemV4 2 color)/255.0) * 2.0 ) (((getElemV4 1 color)/255.0) * 2.0 ) (((getElemV4 0 color)/255.0) * 2.0 )
                        in  rgb


model_specular :: Model -> Vec.Vec2 Double -> Double                        
model_specular model uv =   let (u, v)              =   fromVec2 uv
                                (u', v')            =  (floor (u * (fromIntegral (specwidth $ getSpecularMap model)))  ,  floor ( fromIntegral (specheight $ getSpecularMap model) - v * (fromIntegral (specheight $ getSpecularMap model)) ))
                                image               =     (specimg (getSpecularMap model))
                                ee  =  (pixelAt image u' v')
                            in    (to_double $ fromIntegral ee)



-- model_face_normal :: Model -> Int -> Vec.Vec3 Double
-- model_face_normal model iface = let (v0,v1,v2) = mapTuple3 ((model_vert model iface)) (0, 1, 2)
--                                 in  Vec.normalize $ Vec.cross (v2 - v1) (v1 - v0)

                        
valid_obj_num :: String  -> Bool
valid_obj_num ""  = False
valid_obj_num xs  =
  case dropWhile Char.isDigit xs of
    ""       -> True
    ('.':ys) -> valid_obj_num ys
    ('-':ys) -> valid_obj_num ys
    ('e':'-':ys) -> all Char.isDigit ys
    _        -> False


