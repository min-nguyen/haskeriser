{-# LANGUAGE OverloadedStrings #-}

module SDL_Aux
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

sdl_renderframe :: Screen -> IO ()
sdl_renderframe t_screen = do
                        t <- SDL.updateTexture (texture t_screen) Nothing (buffer t_screen) ((width t_screen))
                        let screen' = t_screen {texture = t}
                        cl <- SDL.clear (renderer screen')
                        co <- SDL.copy (renderer screen') (texture screen') Nothing Nothing
                        pres <- SDL.present (renderer screen')
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

sdl_put_pixel :: Screen -> CInt -> CInt -> Vec3D -> IO Screen
sdl_put_pixel screen x y color = do
                            let success = (x < 0 || x >= width screen || y < 0 || y >= height screen)
                            case success of
                              True -> return screen
                              False -> do
                                    let r = GLM.clamp (255*((Vec.toList color) !! 0)) 0 255
                                    let g = GLM.clamp (255*((Vec.toList color) !! 1)) 0 255
                                    let b = GLM.clamp (255*((Vec.toList color) !! 2)) 0 255
                                    let uscreen = Byte.unpack ( buffer screen)
                                    let (xs,ys) = splitAt (fromIntegral $ toInteger (y*(width screen) + x)) uscreen
                                    let insert =  ( (Bit.shift 24 128)) Bit..|.  (Bit.shift (toInteger $ floor r) 16) Bit..|.  (Bit.shift (toInteger $ floor g) 8) Bit..|. (toInteger $ floor b)
                                    let newscreen = screen {buffer = Byte.pack (xs ++ ( fromInteger insert: ys))}
                                    return newscreen
-- sdl_setcolors :: 