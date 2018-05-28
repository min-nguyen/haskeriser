{-# LANGUAGE OverloadedStrings #-}

module Main 
    where 

import GLM as GLM
import SDL_Aux as SDLAux
import qualified SDL as SDL 
import Control.Monad (forM, unless, void)

loop :: Screen -> SDL.Texture -> SDL.Window -> IO ()
loop screen text window  = do
  poll <- SDL.pollEvents
  -- If elapsed time > 100, update game
  let quit = elem SDL.QuitEvent $ map SDL.eventPayload poll

  sdl_renderframe screen
--   SDL.present (renderer screen)
  unless quit $ loop screen text window


main :: IO ()
main = do
    -- window & surface & renderer
    screen <- sdl_init

    loop screen (texture screen) (window screen)

    SDL.destroyWindow $ window screen
    SDL.quit
        