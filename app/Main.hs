{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

        -- |‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾| -- 
        -- |                                                                        | -- 
        -- |                                HASKERIZER                              | -- 
        -- |                                                                        | -- 
        -- |                                                                        | -- 
        --  ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾  -- 


module Main 
    where

import Prelude hiding (any, mapM_)
import Control.Monad hiding (mapM_)
import Data.Foldable hiding (elem)
import Data.Maybe
import Data.Word8
import Data.Vec as Vec ( Mat44, identity )
import Data.Matrix
import Foreign.C.Types
import SDL.Vect
import SDL (($=))
import qualified SDL
import Triangle
import SDLx
import Renderer
import Model
import Camera
import TGA
import Light
import Rasteriser
import Shader
import Types
import Text.Printf
import Control.Exception
import System.CPUTime

loop :: (Rasteriser -> Shader -> IO((Rasteriser, Shader))) -> Model -> Light -> Camera -> Shader -> Shader -> Shader -> IO()
loop draw_func model light camera camera_shader directional_shader ambient_shader = do
        screen <- sdl_init
        start <- getCPUTime
        let ras = (load_rasteriser model screen camera light) :: Rasteriser
            loop' = do
                        events <- map SDL.eventPayload <$> SDL.pollEvents
                        let quit = SDL.QuitEvent `elem` events

                        SDL.rendererDrawColor (renderer screen) $= V4 maxBound maxBound maxBound maxBound
                        SDL.clear (renderer screen)
                        -- (ras'  , ambient_shader')    <- draw_func ras ambient_shader (Vec.identity :: Mat44 Double)
                      
                        initial_dir_shader <- setup_shader ras directional_shader (Vec.identity :: Mat44 Double)

                        camera_shaderx      <- setup_shader ras camera_shader (getMVP initial_dir_shader)
                        let directional_shaderx = initial_dir_shader {getTransformM = getMVP camera_shaderx}

                        -- (ras'  , directional_shader')  <- draw_func ras  directional_shaderx 
                     
                        (ras'' , camera_shader') <- draw_func ras camera_shaderx 
               
                        sequence [render_screen ras'' camera_shader' camera_shaderx px py | py <- [0 .. screenHeight_i - 1], px <- [0 .. screenWidth_i - 1]]
              
                        SDL.present (renderer screen) 
        loop'
        end   <- getCPUTime
        let diff = (fromIntegral (end - start)) / (10^12)
        printf "Computation time: %0.3f sec\n" (diff :: Double)
        ----- One loop freeze ----
        let loop'' = do
                        events <- map SDL.eventPayload <$> SDL.pollEvents
                        let quit = SDL.QuitEvent `elem` events
                        unless quit (loop'')
        loop''
        --------------------------
        SDL.destroyWindow (window screen)
        SDL.quit
             
main :: IO ()
main = do
    model <- load_model
    camera <- load_camera
    light <- load_light
    let directional_shader = load_directionalshader
        ambient_shader = load_ambientshader
        camera_shader = load_camerashader
    loop draw_loop model light camera camera_shader directional_shader ambient_shader
    return ()
