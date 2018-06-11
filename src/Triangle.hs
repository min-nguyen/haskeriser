{-# LANGUAGE OverloadedStrings #-}

module Triangle 
    where

import Prelude hiding (any, mapM_)
import Control.Monad hiding (mapM_)
import Data.Foldable hiding (elem)
import Data.Maybe
import Data.Word8
import Data.Matrix
import SDL.Vect
import SDL (($=))
import qualified SDL
import Matrix as Matrix
import SDL_Aux


data Triangle = Triangle {  points  :: (V4 Double, V4 Double, V4 Double),
                            color   :: V4 Word8 }

data Color = Red | Blue | Yellow | Green | White | Purple

get_color :: Color -> V4 Word8 
get_color color = case color of 
    Red     -> V4 190  37  37  255
    Yellow  -> V4 190 190  37  255
    Green   -> V4 37  190  37  255
    Blue    -> V4 25  25   180 255
    Purple  -> V4 190 37   190 255
    White   -> V4 190 190  190 255

scale_vec4 :: V4 Double -> V4 Double    
scale_vec4 v4 =  let (V4 x y z w) = v4 * (2/555) - (V4 1 1 1 1)
                 in (V4 (-x) (-y) z 1)
