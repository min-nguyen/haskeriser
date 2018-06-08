{-# LANGUAGE OverloadedStrings #-}

module Camera
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

data Camera = Camera {  position :: V4 Double }

load_camera :: IO Camera
load_camera = do
    return $ Camera (V4 0 0 (-0.5) 1)   