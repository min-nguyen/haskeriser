{-# LANGUAGE OverloadedStrings #-}

        -- |‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾| -- 
        -- |                                                                        | -- 
        -- |                        CAMERA ATTRIBUTES/FUNCTIONS                     | -- 
        -- |                                                                        | -- 
        -- |                                                                        | -- 
        --  ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾  -- 

module Camera
    where

import Prelude hiding (any, mapM_)
import Control.Monad hiding (mapM_)
import Data.Foldable hiding (elem)
import Data.Maybe
import Data.Word8
import Data.List
import Data.Vec as Vec
import Data.Cross
import Foreign.C.Types
import SDL.Vect
import SDL (($=))
import qualified SDL
import SDLx
import Control.Lens
import Types
import Util

load_camera :: IO Camera
load_camera = do
    return $ Camera (toVec3  (1.0) (1.0) (-90.0)) (toVec3  (0.0) (-1.0) (180.0)) -- (rotateTowards 180 0 0)
                        -- order == y z x ??

rotateTowards ::  Double ->  Double -> Double -> Mat44 Double
rotateTowards axisx axisy axisz = multmm (Vec.rotationZ (axisz/180)) (multmm (Vec.rotationY (axisy/180)) (Vec.rotationX (axisx/180)))