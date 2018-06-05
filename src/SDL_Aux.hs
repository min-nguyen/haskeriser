{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

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

screenWidth, screenHeight :: CInt
(screenWidth, screenHeight) = (800, 800)

-- # SDL.Texture, (Width, Height)
data Texture = Texture SDL.Texture (V2 CInt)
-- # Screen
data Screen = Screen  { window    :: SDL.Window,
                        renderer  :: SDL.Renderer,
                        texture   :: Texture,
                        height    :: CInt,
                        width     :: CInt,
                        center    :: Point V2 CInt
                      }

                      
sdl_draw_line :: Screen -> V2 CInt -> V2 CInt -> V4 Word8 -> IO ()
sdl_draw_line screen xy1 xy2 (V4 r g b a) = do
    SDL.rendererDrawColor (renderer screen) $= V4 r g b a
    SDL.drawLine (renderer screen) (P xy1) (P xy2)                   

sdl_put_pixel :: Screen -> V2 CInt -> V4 Word8 -> IO ()
sdl_put_pixel screen xy (V4 r g b a) = do
    SDL.rendererDrawColor (renderer screen) $= V4 r g b a
    -- print $ "drawing" ++ (show xy)
    SDL.drawPoint (renderer screen) (P xy)

sdl_create_blank :: SDL.Renderer -> V2 CInt -> SDL.TextureAccess -> IO Texture
sdl_create_blank r sz access = Texture <$> SDL.createTexture r SDL.RGBA8888 access sz <*> pure sz

sdl_render_texture :: SDL.Renderer -> Texture -> Point V2 CInt -> Maybe (SDL.Rectangle CInt) -> Maybe CDouble -> Maybe (Point V2 CInt) -> Maybe (V2 Bool) -> IO ()
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
      SDL.defaultWindow {SDL.windowInitialSize = V2 screenWidth screenHeight}
  SDL.showWindow window

  renderer <-
    SDL.createRenderer
      window
      (-1)
      SDL.RendererConfig
        { SDL.rendererType = SDL.AcceleratedVSyncRenderer
        , SDL.rendererTargetTexture = False
        }

  SDL.rendererDrawColor renderer $= V4 maxBound maxBound maxBound maxBound

  targetTexture <- sdl_create_blank renderer (V2 screenWidth screenHeight) SDL.TextureAccessTarget
  let screenCenter = P (V2 (screenWidth `div` 2) (screenHeight `div` 2))
  return $ Screen window renderer targetTexture screenHeight screenWidth screenCenter


sdl_noquit :: IO () -> IO ()
sdl_noquit next = do
                poll <- SDL.pollEvents
                let quit = elem SDL.QuitEvent $ map SDL.eventPayload poll
                return $ unless quit $ next
                return ()
