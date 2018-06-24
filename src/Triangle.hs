{-# LANGUAGE OverloadedStrings #-}

module Triangle 
    where

import Prelude hiding (any, mapM_)
import Control.Monad hiding (mapM_)
import Data.Foldable hiding (elem)
import Data.Maybe
import Data.Word8
import SDL.Vect
import SDL (($=))
import qualified SDL
import Matrix as Matrix
import SDLx
import Data.Vec as Vec
import Types

get_color :: Color -> Vec4 Word8 
get_color color = case color of 
    Red     -> Vec.fromList  [190,  37,  37 , 255]
    Yellow  -> Vec.fromList  [190, 190,  37 , 255]
    Green   -> Vec.fromList  [37,  190,  37  ,255]
    Blue    -> Vec.fromList  [25 , 25 ,  180 ,255]
    Purple  -> Vec.fromList  [190, 37 ,  190 ,255]
    White   -> Vec.fromList  [190, 190 , 190 ,255]

