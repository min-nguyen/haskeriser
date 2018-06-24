{-# LANGUAGE OverloadedStrings #-}

module Main 
    where

import Prelude hiding (any, mapM_)
import Control.Monad hiding (mapM_)
import Data.Foldable hiding (elem)
import Data.Maybe
import Data.Word8
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

loop :: (Rasteriser -> Shader -> IO()) -> Model -> Light -> Camera -> Shader -> IO()
loop draw_func model light camera shader = do
        screen <- sdl_init
        let rasteriser = Rasteriser model screen camera light
            loop' = do
                        events <- map SDL.eventPayload <$> SDL.pollEvents
                        let quit = SDL.QuitEvent `elem` events

                        SDL.rendererDrawColor (renderer screen) $= V4 maxBound maxBound maxBound maxBound
                        SDL.clear (renderer screen)
                        draw_func rasteriser shader
                      
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
    let shader = load_shader
    loop draw_loop model light camera shader
    return ()
