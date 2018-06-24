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
import Data.Cross
import Foreign.C.Types
import SDL.Vect
import SDL (($=))
import qualified SDL
import SDLx
import Control.Lens
import Data.List.Split
import Data.Vec as Vec
import Types
import Util

load_light :: IO Light
load_light = return $ Light (toVec3D 0.0 0.0 (-1.0))