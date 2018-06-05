{-# LANGUAGE OverloadedStrings #-}

module Scene 
    where

import Prelude hiding (any, mapM_)
import Control.Monad hiding (mapM_)
import Data.Foldable hiding (elem)
import Data.Maybe
import Data.Word8
import Data.Matrix
-- import Foreign.qC.Types
import SDL.Vect
import SDL (($=))
import qualified SDL
import qualified GLM as GLM
import SDL_Aux

data Triangle = Triangle {  points  :: (V4 Double, V4 Double, V4 Double),
                            color   :: V4 Word8 }

data Color = Red | Blue | Yellow | Green | White | Purple 

data Camera = Camera {  position :: V4 Double }

cam_projection_matrix :: Camera -> Matrix Double
cam_projection_matrix cam = fromList 2 2 [1, 0, 0, 0,
                                          0, 1, 0, 0,
                                          0, 0, 1, 0,
                                          0, 0, -1/z, 1]
                            where V4 x y z w = position cam

get_color :: Color -> V4 Word8 
get_color color = case color of 
    Red     -> V4 190  37  37  1
    Yellow  -> V4 190 190  37  1
    Green   -> V4 37  190  37  1
    Blue    -> V4 25  25   180 1
    Purple  -> V4 190 37   190 1
    White   -> V4 190 190  190 1

loadCamera :: IO Camera
loadCamera = do
    return $ Camera (V4 0 0 (-0.5) 1)    

loadTriangles :: IO ([Triangle])
loadTriangles = do
    let vA = (V4 555  0  0  1)
        vB = (V4 0  0  0  1)
        vC = (V4 555  0  555  1)
        vD = (V4 0  0  555  1)
        vE = (V4 555  555  0  1)
        vF = (V4 0  555  0  1)
        vG = (V4 555  555  555  1)
        vH = (V4 0  555  555  1)
        tri_1   = Triangle (vC, vB, vA) (get_color Red)
        tri_2   = Triangle (vC, vD, vB) (get_color Red)
        tri_3   = Triangle (vA, vE, vC) (get_color Blue)
        tri_4   = Triangle (vC, vE, vG) (get_color Blue)
        tri_5   = Triangle (vF, vB, vD) (get_color Purple)
        tri_6   = Triangle (vH, vF, vD) (get_color Purple)
        tri_7   = Triangle (vE, vF, vG) (get_color Yellow)
        tri_8   = Triangle (vF, vH, vG) (get_color Yellow)
        tri_9   = Triangle (vG, vD, vC) (get_color Green)
        tri_10  = Triangle (vG, vH, vD) (get_color Green)
        qA = V4 290 0 114 1;
        qB = V4 130 0  65 1;
        qC = V4 240 0 272 1;
        qD = V4  82 0 225 1;
        qE = V4 290 165 114 1;
        qF = V4 130 165  65 1;
        qG = V4 240 165 272 1;
        qH = V4  82 165 225 1;
        tri_11 = Triangle(qE, qB, qA) (get_color Red)
        tri_12 = Triangle(qE, qF, qB) (get_color Red)
        tri_13 = Triangle(qF, qD, qB) (get_color Red)
        tri_14 = Triangle(qF, qH, qD) (get_color Red)
        tri_15 = Triangle(qH, qC, qD) (get_color Red)
        tri_16 = Triangle(qH, qG, qC) (get_color Red)
        tri_17 = Triangle(qG, qE, qC) (get_color Red)
        tri_18 = Triangle(qE, qA, qC) (get_color Red)
        tri_19 = Triangle(qG, qF, qE) (get_color Red)
        tri_20 = Triangle(qG, qH, qF) (get_color Red)
   
    return [tri_1, tri_2, tri_3, tri_4, tri_5, tri_6, tri_7, tri_8, tri_9, tri_10, tri_11,
        tri_12, tri_13, tri_14, tri_15, tri_16, tri_17, tri_18, tri_19, tri_20]
