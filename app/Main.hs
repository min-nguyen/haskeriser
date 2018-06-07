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
import Matrix as Matrix
import Triangle
import SDL_Aux
import Renderer
import Model
import Camera
import TGA

loop :: (Screen -> [Triangle] -> Camera -> IO()) -> [Triangle] -> Camera -> IO()
loop draw_func triangles camera = do
        screen <- sdl_init
        let loop' = do
                        events <- map SDL.eventPayload <$> SDL.pollEvents
                        let quit = SDL.QuitEvent `elem` events
                        sdl_set_render_target (renderer screen) (Just $ texture screen)
                        SDL.rendererDrawColor (renderer screen) $= V4 maxBound maxBound maxBound maxBound
                        SDL.clear (renderer screen)
                        draw_func screen  triangles camera 
                        sdl_set_render_target (renderer screen) Nothing
                        sdl_render_texture (renderer screen) (texture screen) 0 Nothing (Just (fromIntegral 0)) (Just $ center screen) Nothing
                        SDL.present (renderer screen)
                        unless quit (loop')
        loop'
        SDL.destroyWindow (window screen)
        SDL.quit


             
main :: IO ()
main = do
    read_tga "resources/african_head_diffuse.tga"
    -- model <- load_model
    -- camera <- loadCamera
    -- loop draw_loop triangles camera
    return ()


test1 :: Screen -> IO ()
test1 screen = do
    let points = [V2 x y | x <- [0 .. 255], y <- [0 .. 255]]
    let colors = [V4 r g 0 255 | r <- [0 .. 255], g <- [0 .. 255]]
    sequence $ map (\(xy, color) -> sdl_put_pixel screen xy color) 
                        (zip points colors)
    return ()
