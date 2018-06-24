{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
 
module SDL_Aux where


import Prelude hiding (any, mapM_)
import Control.Monad hiding (mapM_)
import Data.Foldable hiding (elem)
import Data.Maybe
import Foreign.C.Types
import SDL.Vect
import Data.Word8
import SDL (($=))
import qualified SDL
import Data.Vec
import qualified Data.Vector.Storable as V

screenWidth, screenHeight :: CInt
(screenWidth, screenHeight) = (800, 800)

-- # SDL.Texture, (Width, Height)
data Texture = Texture SDL.Texture (Vec2 CInt)
-- # Screen
data Screen = Screen  { window    :: SDL.Window,
                        renderer  :: SDL.Renderer,
                        texture   :: Texture,
                        height    :: {-# UNPACK #-} !CInt,
                        width     :: {-# UNPACK #-} !CInt,
                        height_i  :: {-# UNPACK #-} !Int,
                        width_i   :: {-# UNPACK #-} !Int,
                        center    :: Point Vec2 CInt
                      }

                      
sdl_draw_line :: Screen -> Vec2 CInt -> Vec2 CInt -> Vec4 Word8 -> IO ()
sdl_draw_line screen xy1 xy2 (Vec4 r g b a) = do
    SDL.rendererDrawColor (renderer screen) $= Vec4 r g b a
    SDL.drawLine (renderer screen) (P xy1) (P xy2)                   

sdl_put_pixel :: Screen -> Vec2 CInt -> Vec4 Word8 -> IO ()
sdl_put_pixel screen xy (Vec4 r g b a) = do
    SDL.rendererDrawColor (renderer screen) $= Vec4 r g b a
    SDL.drawPoint (renderer screen) (P xy)

sdl_create_blank :: SDL.Renderer -> Vec2 CInt -> SDL.TextureAccess -> IO Texture
sdl_create_blank r sz access = Texture <$> SDL.createTexture r SDL.RGBA8888 access sz <*> pure sz

sdl_render_texture :: SDL.Renderer -> Texture -> Point Vec2 CInt -> Maybe (SDL.Rectangle CInt) -> Maybe CDouble -> Maybe (Point Vec2 CInt) -> Maybe (Vec2 Bool) -> IO ()
sdl_render_texture r (Texture t size) xy clip theta center flips =
  let dstSize =
        maybe size (\(SDL.Rectangle _ size') -> size') clip
  in SDL.copyEx r
                t
                clip
                (Just (SDL.Rectangle xy dstSize))
                (fromMaybe 0 theta)
                center
                (fromMaybe (pure False) flips)

sdl_set_render_target :: SDL.Renderer -> Maybe Texture -> IO ()
sdl_set_render_target r Nothing = SDL.rendererRenderTarget r $= Nothing
sdl_set_render_target r (Just (Texture t _)) = SDL.rendererRenderTarget r $= Just t

sdl_init :: IO Screen
sdl_init = do
  SDL.initialize [SDL.InitVideo]

  SDL.HintRenderScaleQuality $= SDL.ScaleLinear
  do  renderQuality <- SDL.get SDL.HintRenderScaleQuality
      when (renderQuality /= SDL.ScaleLinear) $
        putStrLn "Warning: Linear texture filtering not enabled!"

  window <-
    SDL.createWindow
      "Haskerizer"
      SDL.defaultWindow {SDL.windowInitialSize = Vec2 screenWidth screenHeight}
  SDL.showWindow window

  renderer <-
    SDL.createRenderer
      window
      (-1)
      SDL.RendererConfig
        { SDL.rendererType = SDL.AcceleratedVSyncRenderer
        , SDL.rendererTargetTexture = False
        }

  SDL.rendererDrawColor renderer $= Vec4 maxBound maxBound maxBound maxBound

  targetTexture <- sdl_create_blank renderer (Vec2 screenWidth screenHeight) SDL.TextureAccessTarget
  let screenCenter = P (Vec2 (screenWidth `div` 2) (screenHeight `div` 2))
  return $ Screen window renderer targetTexture screenHeight screenWidth (fromIntegral $ toInteger $ screenHeight) (fromIntegral $ toInteger $ screenWidth) screenCenter


sdl_noquit :: IO () -> IO ()
sdl_noquit next = do
                poll <- SDL.pollEvents
                let quit = elem SDL.QuitEvent $ map SDL.eventPayload poll
                return $ unless quit $ next
                return ()
