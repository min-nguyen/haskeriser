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
import SDL_Aux
import Control.Lens
import Matrix
import Data.List.Split

data Model = Model {verts :: [V3 Double],
                    faces :: [[V3 Integer]],
                    norms :: [V3 Double],
                    uvs   :: [V2 Double]}

load_model :: IO Model
load_model = do
    args <- getArgs
    content <- readFile (args !! 0)
    let linesOfFile = lines content
        verts = stringListToV3List $ map (filter isValidNumber) $ map words $ filter (\l -> (case l of (x:y:xs) -> x == 'v' && y == ' '
                                                                                                       _ -> False)) linesOfFile

        vertn = stringListToV3List $ map (filter isValidNumber) $ map words $ filter (\l -> (case l of (x:y:xs) -> x == 'v' && y == 'n'
                                                                                                       _ -> False)) linesOfFile

        uvs   = stringListToV2List $ map (filter isValidNumber) $ map words $ filter (\l -> (case l of (x:y:xs) -> x == 'v' && y == 't'
                                                                                                       _ -> False)) linesOfFile
        
        faces = map (map $ filter isValidNumber) $ 
                    ( map (map words)) $ 
                        map (map $ map (\c -> if (c == '/') then ' ' else c)) $ 
                             map words $ filter (\l -> (case l of (x:xs) -> x == 'f'
                                                                  _ -> False)) linesOfFile
        faces' = splitEvery 3 (stringListToV3ListI [z | x <- faces, z <- x, not (null z)])
                
    return $ Model verts faces' vertn uvs   

-- load_texture :: IO ()
-- load_texture filename = 

model_face :: Model -> Int -> [Integer]
model_face model ind = [x | face_v3 <- (faces model !! ind), let V3 x y z = face_v3]

model_vert :: Model -> Int -> V3 Double
model_vert model ind = (verts model) !! ind

model_uv :: Model -> Int -> Int -> V2 Integer
model_uv model iface nvert =  let V3 x y z = ((faces model !! iface) !! nvert)
                                  V2 x' y' = (uvs model) !! ( fromIntegral y)
                              in V2 (floor x') (floor y') ----- UPDATE THIS

isValidNumber :: String -> Bool
isValidNumber ""  = False
isValidNumber "." = False
isValidNumber xs  =
  case dropWhile Char.isDigit xs of
    ""       -> True
    ('.':ys) -> all Char.isDigit ys
    ('-':ys) -> isValidNumber ys
    _        -> False


