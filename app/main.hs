{-# LANGUAGE OverloadedStrings #-}

module Main 
    where

import Data.Vec as Vec hiding (map)
import GLM as GLM
import SDL_Aux as SDLAux
import qualified SDL as SDL 
import Control.Monad (forM, unless, void)
import Foreign.C.Types

testdraw :: Screen -> CInt -> CInt -> Bool -> IO Screen
testdraw screen i j isWhite
  | j >= (height screen) = return screen
  | i >= (width screen) = testdraw screen 0 (j + 1)
  | isWhite = do
      success <- SDL.rendererDrawColor (renderer screen) 255 255 255 1 
      SDL.renderDrawPoint (renderer screen) i j
      testdraw screen (i+1) j False
  | not isWhite = do
      success <- SDL.rendererDrawColor (renderer screen) 0 0 0 1 
      SDL.renderDrawPoint (renderer screen) i j
      testdraw screen (i+1) j True

loop :: Screen -> SDL.Texture -> SDL.Window -> IO ()
loop screen text window  = do
  poll <- SDL.pollEvents
  -- If elapsed time > 100, update game
  let quit = elem SDL.QuitEvent $ map SDL.eventPayload poll
  screen' <- testdraw screen 0 0 True
  sdl_renderframe screen'
--   SDL.present (renderer screen)
  unless quit $ loop screen' text window


main :: IO ()
main = do
    -- window & surface & renderer
    screen <- sdl_init

    loop screen (texture screen) (window screen)

    SDL.destroyWindow $ window screen
    SDL.quit
        