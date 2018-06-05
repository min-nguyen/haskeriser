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
import Data.Matrix
import Data.Cross
import Foreign.C.Types
import SDL.Vect
import SDL (($=))
import qualified SDL
import SDL_Aux
import Control.Lens


clamp :: Double -> Double -> Double -> Double
clamp x minval maxval = min (max x minval) maxval

toMatV2 :: V2 a -> Matrix a
toMatV2 (V2 a b) = fromList 1 2 [a, b]

toMatV3 :: V3 a -> Matrix a
toMatV3 (V3 a b c) = fromList 1 3 [a, b, c]

toMatV4 :: V4 a -> Matrix a
toMatV4 (V4 a b c d) = fromList 1 4 [a, b, c, d]

fromMatV2 :: Matrix a -> V2 a
fromMatV2 m  = let [va] = map (\[x, y] -> V2 x y) $ toLists m
                in (va)

fromMatV3 :: Matrix a -> V3 a
fromMatV3 m = let [va] = map (\[x, y, z] -> V3 x y z) $ toLists m
                in (va)

fromMatV4 :: Matrix a -> V4 a
fromMatV4 m = let [va] = map (\[x, y, z, w] -> V4 x y z w) $ toLists m
                in (va)

fromMatV2sq :: Matrix a -> (V2 a, V2 a)
fromMatV2sq m  = let [va, vb] = map (\[x, y] -> V2 x y) $ toLists m
                in (va, vb)

fromMatV3sq :: Matrix a -> (V3 a, V3 a, V3 a)
fromMatV3sq m = let [va, vb, vc] = map (\[x, y, z] -> V3 x y z) $ toLists m
                in (va, vb, vc)

fromMatV4sq :: Matrix a -> (V4 a, V4 a, V4 a, V4 a)
fromMatV4sq m = let [va, vb, vc, vd] = map (\[x, y, z, w] -> V4 x y z w) $ toLists m
                in (va, vb, vc, vd)
