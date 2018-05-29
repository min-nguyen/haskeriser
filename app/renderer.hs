{-# LANGUAGE OverloadedStrings #-}

module Renderer 
    where

import Prelude hiding (any, mapM_)
import Control.Monad hiding (mapM_)
import Data.Foldable hiding (elem)
import Data.Maybe
import Data.Word8
import Foreign.C.Types
import SDL.Vect
import SDL (($=))
import qualified SDL
import qualified GLM as GLM
import SDL_Aux
import Scene


draw :: Screen -> V3 Int -> [Triangle] -> IO ()
draw screen origin triangles = do
    
    return ()
