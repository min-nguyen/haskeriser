{-# LANGUAGE OverloadedStrings #-}

module GLM
    where

import Prelude
import Prelude hiding (any, mapM_)
import Control.Monad hiding (mapM_)
import Data.Foldable hiding (elem)
import Data.Maybe
import Data.Word8
import Data.List
import Data.Matrix as Matrix
import Data.Cross
import Foreign.C.Types
import SDL.Vect
import SDL (($=))
import qualified SDL
import SDL_Aux
import Control.Lens


clamp :: Double -> Double -> Double -> Double
clamp x minval maxval = min (max x minval) maxval

toMatV2 :: V2 Double -> Matrix Double
toMatV2 (V2 a b) = fromList 2 1 [a, b]

toMatV3 :: V3 Double -> Matrix Double
toMatV3 (V3 a b c) = fromList 3 1 [a, b, c]

toMatV4 :: V4 Double -> Matrix Double
toMatV4 (V4 a b c d) = fromList 4 1 [a, b, c, d]

fromMatV2 :: Matrix Double -> V2 Double
fromMatV2 m  = case toLists m of (x:xs) -> (\(x:y:ls) -> V2 x y) $ head $ toLists m
                                 [] -> V2 0 0 

fromMatV3 :: Matrix Double -> V3 Double
fromMatV3 m  = case toLists m of (x:xs) -> (\(x:y:z:ls) -> V3 x y z) $ head $ toLists m
                                 [] -> V3 0 0 0 

fromMatV4 :: Matrix Double -> V4 Double
fromMatV4 m  =  case toLists m of (x:xs) -> (\(x:y:z:w:ls) -> V4 x y z w) $ Matrix.toList m
                                  [] -> V4 0 0 0 0
                

fromMatV2sq :: Matrix Double -> (V2 Double, V2 Double)
fromMatV2sq m  = let [va, vb] = map (\[x, y] -> V2 x y) $ toLists m
                in (va, vb)

fromMatV3sq :: Matrix Double -> (V3 Double, V3 Double, V3 Double)
fromMatV3sq m = let [va, vb, vc] = map (\[x, y, z] -> V3 x y z) $ toLists m
                in (va, vb, vc)

fromMatV4sq :: Matrix Double -> (V4 Double, V4 Double, V4 Double, V4 Double)
fromMatV4sq m = let [va, vb, vc, vd] = map (\[x, y, z, w] -> V4 x y z w) $ toLists m
                in (va, vb, vc, vd)
