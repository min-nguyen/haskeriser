{-# LANGUAGE OverloadedStrings #-}

module Main 
    where

import Prelude hiding (any, mapM_)
import Control.Monad hiding (mapM_)
import Data.Foldable hiding (elem)
import Data.Maybe
import Foreign.C.Types
import SDL.Vect
import SDL (($=))
import qualified SDL
import qualified GLM as GLM
import SDL_Aux




loop :: Screen -> SDL.Texture -> SDL.Window -> IO ()
loop screen text window  = do
  poll <- SDL.pollEvents
  -- If elapsed time > 100, update game
  let quit = elem SDL.QuitEvent $ map SDL.eventPayload poll
--   screen' <- testdraw screen 0 0 True
--   sdl_renderframe screen'
--   SDL.present (renderer screen)
  unless quit $ loop screen text window



draw :: Screen -> Int -> IO ()
draw (Screen window renderer targetTexture screenWidth screenHeight screenCenter) theta = do             


    SDL.rendererDrawColor renderer $= V4 maxBound maxBound maxBound maxBound
    SDL.clear renderer

    SDL.rendererDrawColor renderer $= V4 maxBound 0 0 maxBound
    SDL.fillRect renderer (Just $ SDL.Rectangle (P $ V2 (screenWidth `div` 4) (screenHeight `div` 4))
                                                        (V2 (screenWidth `div` 2) (screenHeight `div` 2)))

    SDL.rendererDrawColor renderer $= V4 0 0 maxBound maxBound
    SDL.drawRect renderer (Just (SDL.Rectangle (P $ V2 (screenWidth `div` 6) (screenHeight `div` 6))
                                                    (V2 (screenWidth * 2 `div` 3) (screenHeight * 2 `div` 3))))

    SDL.rendererDrawColor renderer $= V4 0 maxBound 0 maxBound
    SDL.drawLine renderer (P (V2 0 (screenHeight `div` 2))) (P (V2 screenWidth (screenHeight `div` 2)))

    SDL.rendererDrawColor renderer $= V4 maxBound maxBound maxBound maxBound
    for_ [0, 4 .. screenHeight] $ \i ->
        SDL.drawPoint renderer (P (V2 (screenWidth `div` 2) i))

   
    return ()    

main :: IO ()
main = do
    -- window & surface & renderer
    screen <- sdl_init
    let screenCenter = P (V2 ((width screen) `div` 2) ((height screen) `div` 2))
        loop theta = do
            events <- map SDL.eventPayload <$> SDL.pollEvents
            let quit = SDL.QuitEvent `elem` events
            sdl_set_render_target (renderer screen) (Just $ texture screen)

            draw screen theta

            sdl_set_render_target (renderer screen) Nothing
            sdl_render_texture (renderer screen) (texture screen) 0 Nothing (Just (fromIntegral theta)) (Just $ center screen) Nothing
            SDL.present (renderer screen)
            unless quit (loop (theta + 2 `mod` 360))
    
    loop (0 :: Int)

    SDL.destroyWindow (window screen)
    SDL.quit