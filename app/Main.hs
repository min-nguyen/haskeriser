{-# LANGUAGE OverloadedStrings #-}

module SDL_Auxiliary 
    where

import Control.Concurrent (threadDelay)
import Foreign.C.Types
import qualified Data.Word8 as Word
import Data.Vec as Vec hiding (map)
import Data.StateVar
import Control.Applicative
import Control.Exception
import Control.Monad (forM, unless, void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans
import GLM as GLM
import qualified Data.Bits as Bit
import Data.Data (Data)
import Data.Foldable
import qualified Data.ByteString as Byte
import Data.Maybe (isJust, fromMaybe)
import Data.Monoid (First(..))
import Data.Text (Text)
import Data.Typeable
import Debug.Trace
import SDL.Vect hiding (column)
import qualified SDL as SDL 
import System.FilePath.Posix
import System.Random
import Control.Monad.State as ST
import qualified SDL.Raw.Timer as Raw
import qualified SDL.Raw.Types as Raw

screenWidth, screenHeight :: CInt
(screenWidth, screenHeight) = (800, 800)

-- SDL.Rectangle  = Point (x,y) -> (w,h) -> Rectangle Int
-- SDL.Rectangle :: Point V2 a -> V2 a -> SDL.Rectangle a
-- P  :: f a -> Point f a
-- V2 :: a -> a -> V2 a


-- # SDL.Texture, (Width, Height)
data Texture = Texture SDL.Texture (V2 CInt)
-- # Screen
data Screen = Screen  { window    :: SDL.Window,
                        renderer  :: SDL.Renderer,
                        surface   :: SDL.Surface,
                        texture   :: SDL.Texture,
                        buffer    :: Byte.ByteString,
                        height    :: CInt,
                        width     :: CInt
                      }

load_texture :: SDL.Renderer -> FilePath -> IO Texture
load_texture r filePath = do
  surface <- SDL.loadBMP filePath
  size <- SDL.surfaceDimensions surface
  let key = V4 0 maxBound maxBound maxBound
  SDL.surfaceColorKey surface $= Just key
  t <- SDL.createTextureFromSurface r surface
  SDL.freeSurface surface
  return (Texture t size)

-- Renderer-> Texture -> Point(drawX, drawY) -> Maybe (SDL.Rectangle CInt) -> IO ()
render_texture :: SDL.Renderer -> Texture -> Point V2 CInt -> Maybe (SDL.Rectangle CInt) -> IO ()
render_texture r (Texture t size) xy clip =
  let dstSize = maybe size (\(SDL.Rectangle _ size') ->  size') clip
  in SDL.copy r t clip (Just (SDL.Rectangle xy dstSize))


game_over :: IO ()     
game_over = do
  putStrLn "GAME OVER"
  game_over

loop :: SDL.Renderer -> SDL.Texture -> SDL.Window -> IO ()
loop rend text window  = do
  poll <- SDL.pollEvents
  -- If elapsed time > 100, update game
  let quit = elem SDL.QuitEvent $ map SDL.eventPayload poll
  SDL.rendererDrawColor rend $= V4 maxBound maxBound maxBound maxBound
  SDL.clear rend

  -- Draw sprite at current position
  -- renderTexture rend text pv2 rect

  SDL.present rend
  unless quit $ loop rend text window


sdl_renderframe :: Screen -> IO ()
sdl_renderframe t_screen = do
                        t <- SDL.updateTexture (texture t_screen) Nothing (buffer t_screen) ((width t_screen) * 32)
                        cl <- SDL.clear (renderer t_screen)
                        co <- SDL.copy (renderer t_screen) (texture t_screen) Nothing Nothing
                        pres <- SDL.present (renderer t_screen)
                        return pres

sdl_noquit :: IO () -> IO ()
sdl_noquit next = do
                  poll <- SDL.pollEvents
                  let quit = elem SDL.QuitEvent $ map SDL.eventPayload poll
                  return $ unless quit $ next
                  return ()

sdl_init :: IO (Screen)
sdl_init = do
      SDL.initialize [SDL.InitVideo]
      SDL.HintRenderScaleQuality $= SDL.ScaleLinear
      -- Create window
      window  <- SDL.createWindow "My Window" SDL.defaultWindow { SDL.windowInitialSize = V2 screenWidth screenHeight }
      SDL.showWindow window
      -- Create surface
      surface <- SDL.getWindowSurface window
      -- Fill window
      let white =  V4 maxBound maxBound maxBound maxBound 
      SDL.surfaceFillRect surface Nothing white
      renderer <- SDL.createRenderer window (-1) SDL.RendererConfig { 
        SDL.rendererType = SDL.AcceleratedRenderer,
        SDL.rendererTargetTexture = False
      }
      texture <- SDL.createTexture renderer SDL.ARGB8888 SDL.TextureAccessStatic (V2 screenWidth screenHeight)
      SDL.rendererDrawColor renderer $= V4 maxBound maxBound maxBound maxBound
      let buffer = Byte.pack $ [0 | j <- [0 .. screenHeight*screenWidth]]
      let screen = Screen window renderer surface texture buffer screenHeight screenWidth
      return screen

put_pixel :: Screen -> CInt -> CInt -> Vec3 Double -> IO Screen
put_pixel screen x y color = do
                            let success = (x < 0 || x >= width screen || y < 0 || y >= height screen)
                            let r = GLM.clamp (255*((Vec.toList color) !! 0)) 0 255
                            let g = GLM.clamp (255*((Vec.toList color) !! 1)) 0 255
                            let b = GLM.clamp (255*((Vec.toList color) !! 2)) 0 255
                            let uscreen = Byte.unpack ( buffer screen)
                            let (xs,ys) = splitAt (fromIntegral $ toInteger (y*(width screen) + x)) uscreen
                            let insert =  ( (Bit.shift 24 128)) Bit..|.  (Bit.shift (toInteger $ floor r) 16) Bit..|.  (Bit.shift (toInteger $ floor g) 8) Bit..|. (toInteger $ floor b)
                            let newbuffer = xs ++ ( fromInteger insert: ys)
                            return screen

main :: IO ()
main = do
  -- window & surface & renderer
  screen <- sdl_init

 

  SDL.destroyWindow $ window screen
  SDL.quit

  -- # loop renderer (True, (0,0)) window 
  -- loop renderer text window


  -- spriteSheetTexture <- load_texture renderer "../assets/SF.bmp"
  -- let draw = renderTexture renderer
  --     game = Game 0 False False
  --     coin = ((500, 500), 2)
  --     -- Load Snake
  --     spriteSize = V2 cellWidth cellHeight
  --     clip1 = SDL.Rectangle (P (V2 0 10)) spriteSize
  --     clip2 = SDL.Rectangle (P (V2 50 10)) spriteSize
  --     clip3 = SDL.Rectangle (P (V2 100 10)) spriteSize
  --     clip4 = SDL.Rectangle (P (V2 150 10)) spriteSize
  --     --Define character sprite loading functions
  --     sprite = Sprite $ \state -> case state of 
  --       _ -> (spriteSheetTexture, [clip1, clip2, clip3, clip4])


 