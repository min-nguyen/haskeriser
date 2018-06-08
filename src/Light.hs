{-# LANGUAGE OverloadedStrings #-}

module Light
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
import Data.List.Split

-- data Light = Light {position    :: V3 Double,
--                     direction   :: V3 Double,
--                     power       :: Double}
data Light = Light {direction   :: V3 Double}


load_light :: IO Light
load_light = return $ Light (V3 0 (-1) 0)