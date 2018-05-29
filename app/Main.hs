{-# LANGUAGE OverloadedStrings #-}

module Main 
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

loop :: (Screen -> IO ()) -> IO()
loop draw_func = do
        screen <- sdl_init
        let loop' = do
                        events <- map SDL.eventPayload <$> SDL.pollEvents
                        let quit = SDL.QuitEvent `elem` events
                        sdl_set_render_target (renderer screen) (Just $ texture screen)
                        SDL.rendererDrawColor (renderer screen) $= V4 maxBound maxBound maxBound maxBound
                        SDL.clear (renderer screen)
                        draw_func screen
                        sdl_set_render_target (renderer screen) Nothing
                        sdl_render_texture (renderer screen) (texture screen) 0 Nothing (Just (fromIntegral 0)) (Just $ center screen) Nothing
                        SDL.present (renderer screen)
                        unless quit (loop')
        loop'
        SDL.destroyWindow (window screen)
        SDL.quit

main :: IO ()
main = do
        loop test1



        -- main :: IO ()
        -- main =  do
        --     triangles <- loadTriangles
        --     camera <- loadCamera
test1 :: Screen -> IO ()
test1 screen = do
    let points = [V2 x y | x <- [0 .. 255], y <- [0 .. 255]]
    let colors = [V4 r g 0 255 | r <- [0 .. 255], g <- [0 .. 255]]
    sequence $ map (\(xy, color) -> sdl_put_pixel screen xy color) 
                        (zip points colors)
    return ()

-- test2 :: Screen -> Int -> IO ()
-- test2 (Screen window renderer targetTexture screenWidth screenHeight screenCenter) theta = do             

--     SDL.rendererDrawColor renderer $= V4 maxBound 0 0 maxBound
--     SDL.fillRect renderer (Just $ SDL.Rectangle (P $ V2 (screenWidth `div` 4) (screenHeight `div` 4))
--                                                         (V2 (screenWidth `div` 2) (screenHeight `div` 2)))

--     SDL.rendererDrawColor renderer $= V4 0 0 maxBound maxBound
--     SDL.drawRect renderer (Just (SDL.Rectangle (P $ V2 (screenWidth `div` 6) (screenHeight `div` 6))
--                                                     (V2 (screenWidth * 2 `div` 3) (screenHeight * 2 `div` 3))))

--     SDL.rendererDrawColor renderer $= V4 0 maxBound 0 maxBound
--     SDL.drawLine renderer (P (V2 0 (screenHeight `div` 2))) (P (V2 screenWidth (screenHeight `div` 2)))

--     SDL.rendererDrawColor renderer $= V4 maxBound maxBound maxBound maxBound
--     for_ [0, 4 .. screenHeight] $ \i ->
--         SDL.drawPoint renderer (P (V2 (screenWidth `div` 2) i))
   
--     return ()    
