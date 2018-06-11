{-# LANGUAGE OverloadedStrings #-}

module Util
    where


import Prelude hiding (any, mapM_)
import Control.Monad hiding (mapM_)
import Control.Arrow ((***))
import Data.Foldable hiding (elem)
import Data.Maybe
import Data.Word8
import Data.List
import Data.Cross
import Foreign.C.Types
import SDL.Vect
import SDL (($=))
import qualified SDL
import Matrix as Matrix
import qualified Data.Vector as V


clamp :: Double -> Double -> Double -> Double
clamp x minval maxval = min (max x minval) maxval

stringListToV3List :: [String] -> [V3 Double]
stringListToV3List str = case str of (x:xs) -> let (a:b:c:_) = take 3 (str) in ((V3 (read a) (read b) (read c)):(stringListToV3List $ drop 3 str))
                                     [] -> []

stringListToV2List :: [[String]] -> [V2 Double]
stringListToV2List str = [(V2 (read a) (read b) ) | (a:b:_) <- (str) ]

stringListToV3ListI :: [[String]] -> [V3 Integer]
stringListToV3ListI str = [(V3 (read a) (read b) (read c) ) | (a:b:c:_) <- (str) ]

mapTuple2 :: (a -> b) -> (a, a) -> (b, b)
mapTuple2 f (a1, a2) = (f a1, f a2)

mapTuple3 :: (a -> b) -> (a, a, a) -> (b, b, b)
mapTuple3 f (a1, a2, a3) = (f a1, f a2, f a3)

replaceAt :: a -> Int -> V.Vector a -> V.Vector a 
replaceAt newElement n array = V.take n array V.++  (newElement `V.cons` V.drop (n + 1) array)

to_double :: Int -> Double
to_double x = fromIntegral x

map2 :: (Functor f) => (a -> b) -> f (f a) -> f (f b)
map2 f fa = fmap (fmap f) fa

map3 :: (Functor f) => (a -> b) -> f (f (f a)) -> f (f (f b))
map3 f fa = fmap (fmap (fmap f)) fa