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
import Foreign.C.Types
import SDL.Vect
import SDL (($=))
import qualified SDL
import Control.Lens
import Matrix
import Data.List.Split
import TGA

data Model = Model {verts       :: [(V3 Double, Int)],
                    faces       :: [[(V3 Integer, Int)]],
                    norms       :: [(V3 Double, Int)],
                    uvs         :: [(V2 Double, Int)],
                    diffuse_map :: TGA_Header}

load_model :: IO Model
load_model = do
    args <- getArgs
    content <- readFile (args !! 0)
    let linesOfFile = lines content
    
        verts = stringListToV3List $ map (filter isValidNumber) $ map words $ filter (\l -> (case l of (x:y:xs) -> x == 'v' && y == ' '
                                                                                                       _ -> False)) linesOfFile

        norms = stringListToV3List $ map (filter isValidNumber) $ map words $ filter (\l -> (case l of (x:y:xs) -> x == 'v' && y == 'n'
                                                                                                       _ -> False)) linesOfFile

        uvs   = stringListToV2List $ map (filter isValidNumber) $ map words $ filter (\l -> (case l of (x:y:xs) -> x == 'v' && y == 't'
                                                                                                       _ -> False)) linesOfFile
        
        faces = map (map $ filter isValidNumber) $ 
                    ( map (map words)) $ 
                        map (map $ map (\c -> if (c == '/') then ' ' else c)) $ 
                             map words $ filter (\l -> (case l of (x:xs) -> x == 'f'
                                                                  _ -> False)) linesOfFile
        faces' = splitEvery 3 (stringListToV3ListI [z | x <- faces, z <- x, not (null z)])

        nats = 1 : map (+1) (nats)

        faces'' = zipWith (\f i -> zipWith (\f' i' -> (f', (head i)+i')) f (nats)) faces' (map (\x -> [x]) ((nats :: [Int])) :: [[Int]])
        verts'' = zipWith (\f i -> (f, i)) verts (nats :: [Int])
        norms'' = zipWith (\f i -> (f, i)) norms (nats :: [Int])
        uvs'' = zipWith (\f i -> (f, i)) uvs (nats :: [Int])


    print faces''
    diffuse_map <- read_tga "resources/african_head_diffuse.tga"    
    return $ Model verts'' faces'' norms'' uvs''   diffuse_map



model_face :: Model -> Int -> [Integer]
model_face model ind = [x | face_v3 <- ((faces model !! ind)), let V3 x y z = fst face_v3]

model_vert :: Model -> Int -> V3 Double
model_vert model ind = fst $ (verts model) !! ind

model_uv :: Model -> Int -> Int -> V2 Integer
model_uv model iface nvert =  let V3 x y z = fst $ ( ( (faces model)) !! iface) !! nvert
                                  V2 x' y' = fst $ (uvs model) !! ( fromIntegral y)
                              in V2 (floor x' * (toInteger $ width $ diffuse_map model)) (floor y' * (toInteger $ height $ diffuse_map model)) ----- UPDATE THIS

isValidNumber :: String -> Bool
isValidNumber ""  = False
isValidNumber "." = False
isValidNumber xs  =
  case dropWhile Char.isDigit xs of
    ""       -> True
    ('.':ys) -> all Char.isDigit ys
    ('-':ys) -> isValidNumber ys
    _        -> False


