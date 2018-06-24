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
import qualified Data.Vector as V
import Data.Vec as Vec hiding (take, drop, foldr, map)
import qualified Data.Vec as Vec (take, drop, foldr, map)
import Codec.Picture
import Codec.Picture.Types

clamp :: Double -> Double -> Double -> Double
clamp x minval maxval = min (max x minval) maxval

stringListToVec3List :: [String] -> [Vec3 Double]
stringListToVec3List str = case str of (x:xs) -> let (a:b:c:_) = take 3 (str) in ((Vec.fromList [read a,read b,read c]):(stringListToVec3List $ drop 3 str))
                                       [] -> []

stringListToVec2List :: [[String]] -> [Vec2 Double]
stringListToVec2List str = [(Vec.fromList [ (read a) ,(read b) ]) | (a:b:_) <- (str) ]

stringListToVec3ListI :: [[String]] -> [Vec3 Integer]
stringListToVec3ListI str = [(Vec.fromList [ (read a) ,(read b) , (read c)] ) | (a:b:c:_) <- (str) ]

mapTuple2 :: (a -> b) -> (a, a) -> (b, b)
mapTuple2 f (a1, a2) = (f a1, f a2)

mapTuple3 :: (a -> b) -> (a, a, a) -> (b, b, b)
mapTuple3 f (a1, a2, a3) = (f a1, f a2, f a3)

replaceAt :: a -> Int -> V.Vector a -> V.Vector a 
replaceAt newElement n array = (V.take n array) V.++  (newElement `V.cons` V.drop (n + 1) array)

to_double :: Int -> Double
to_double x = fromIntegral x

map2 :: (Functor f) => (a -> b) -> f (f a) -> f (f b)
map2 f fa = fmap (fmap f) fa

map3 :: (Functor f) => (a -> b) -> f (f (f a)) -> f (f (f b))
map3 f fa = fmap (fmap (fmap f)) fa

listToTuple3 :: [a] -> (a, a, a)
listToTuple3 as = (as !! 0, as !! 1, as !! 2)

toVec2D :: Double -> Double -> Vec2 Double
toVec2D x y = Vec.fromList [x,y]

toVec3D ::  Double -> Double -> Double -> Vec3 Double
toVec3D x y z = Vec.fromList [x,y,z]

toVec4D ::  Double -> Double -> Double -> Double -> Vec4 Double
toVec4D x y z w = Vec.fromList [x,y,z,w]

toVec2 :: (Num a) =>   a -> a -> Vec2 a
toVec2 x y = Vec.fromList [x,y]

toVec3 :: (Num a) =>   a -> a -> a -> Vec3 a
toVec3 x y z = Vec.fromList [x,y,z]

toVec4 :: (Num a) =>   a -> a -> a -> a -> Vec4 a
toVec4 x y z w = Vec.fromList [x,y,z,w]

fromVec2D :: Vec2 Double -> ( Double , Double )
fromVec2D xy =  let [x,y] = Vec.toList xy
                in (x,y)

fromVec3D :: Vec3 Double ->  (Double , Double, Double) 
fromVec3D xyz = let [x,y,z] = Vec.toList xyz
                in (x,y,z)

fromVec4D :: Vec4 Double ->  (Double , Double, Double, Double)
fromVec4D xyzw = let [x,y,z,w] = Vec.toList xyzw 
                in (x,y,z,w)

fromVec2 ::(Num a) =>   Vec2 a -> ( a , a )
fromVec2 xy =  let [x,y] = Vec.toList xy
                in (x,y)

fromVec3 ::(Num a) =>   Vec3 a ->  (a , a, a) 
fromVec3 xyz = let [x,y,z] = Vec.toList xyz
                in (x,y,z)

fromVec4 ::(Num a) =>   Vec4 a -> (a, a, a, a)
fromVec4 xyzw = let [x,y,z,w] = Vec.toList xyzw 
                in (x,y,z,w)

embedVec3to4 :: (Num a) =>  Vec3 a -> Vec4 a
embedVec3to4 v3 = let (x,y,z) = fromVec3 v3 in toVec4 x y z 1

projectVec4to3 :: (Num a) => Vec4 a -> Vec3 a
projectVec4to3 v4 = let (x,y,z,w) = fromVec4 v4 in toVec3 x y z

projectVec3to2 :: (Num a) => Vec3 a -> Vec2 a
projectVec3to2 v3 = let (x,y,z) = fromVec3 v3 in toVec2 x y 

projectVec4to2 :: (Num a) => Vec4 a -> Vec2 a
projectVec4to2 v4 = let (x,y,z,w) = fromVec4 v4 in toVec2 x y 


embedVec3to4D ::  Vec3 Double -> Vec4 Double
embedVec3to4D v3 = let (x,y,z) = fromVec3D v3 in toVec4D x y z 1

projectVec4to3D :: Vec4 Double -> Vec3 Double
projectVec4to3D v4 = let (x,y,z,w) = fromVec4D v4 in toVec3D x y z


multms3 :: (Num a) => Vec3 a -> a -> Vec3 a
multms3 m s = let vs = Vec.toList m
              in Vec.fromList $ map (s*) vs

multms4 :: (Num a) => Vec4 a -> a -> Vec4 a
multms4 m s = let vs = Vec.toList m
              in Vec.fromList $ map (s*) vs


reduce_zbuffer :: [V.Vector (Double, Vec.Vec4 Word8)] ->  (V.Vector (Double, Vec4 Word8))
reduce_zbuffer zbuffers =  foldr (\veca vecb -> V.map (\((zindex1, rgba1),(zindex2, rgba2)) -> if zindex1 > zindex2 then (zindex1, rgba1) else (zindex2, rgba2)) (V.zip veca vecb) ) V.empty zbuffers