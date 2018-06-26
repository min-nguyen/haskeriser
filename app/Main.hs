{-# LANGUAGE OverloadedStrings #-}


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
import Data.Vec ( Mat44 )
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

loop :: (Rasteriser -> Shader -> Maybe (Mat44 Double) -> IO((Rasteriser, Shader))) -> Model -> Light -> Camera -> Shader -> Shader -> IO()
loop draw_func model light camera depth_shader camera_shader = do
        screen <- sdl_init
        let ras = (load_rasteriser model screen camera light) :: Rasteriser
            loop' = do
                        events <- map SDL.eventPayload <$> SDL.pollEvents
                        let quit = SDL.QuitEvent `elem` events

                        SDL.rendererDrawColor (renderer screen) $= V4 maxBound maxBound maxBound maxBound
                        SDL.clear (renderer screen)

                        (ras'  , depth_shader')  <- draw_func ras  depth_shader Nothing
                        (ras'' , camera_shader') <- draw_func ras' camera_shader (Just (getMVP depth_shader')) 

                        SDL.present (renderer screen)
                        -- unless quit (loop')
        loop'
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
    let depth_shader = load_depthshader
        camera_shader = load_camerashader
    loop draw_loop model light camera depth_shader camera_shader
    return ()
