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

data Model = Model {verts :: [V3 Double],
                    faces :: [[V3 Integer]],
                    norms :: [V3 Double],
                    uvs   :: [V2 Double]}


isValidNumber :: String -> Bool
isValidNumber ""  = False
isValidNumber "." = False
isValidNumber xs  =
  case dropWhile Char.isDigit xs of
    ""       -> True
    ('.':ys) -> all Char.isDigit ys
    ('-':ys) -> isValidNumber ys
    _        -> False

stringListToV3List :: [[String]] -> [V3 Double]
stringListToV3List str = [(V3 (read a) (read b) (read c)) | (a:b:c:_) <- (str) ]

stringListToV2List :: [[String]] -> [V2 Double]
stringListToV2List str = [(V2 (read a) (read b) ) | (a:b:_) <- (str) ]

stringListToV3List2 :: [[String]] -> [[V3 Integer]]
stringListToV3List2 (xs) = [[(V3 (read a) (read b) (read c))] | (a:b:c:_) <- (xs) ]


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
        faces' = stringListToV3List2 $ [z | x <- faces, z <- x, not (null z)]  
                           
    return $ Model verts faces' vertn uvs   

