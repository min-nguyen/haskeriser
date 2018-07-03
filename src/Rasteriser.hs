{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

        -- |‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾| -- 
        -- |                                                                        | -- 
        -- |                       RASTERIZING OPERATIONS                           | -- 
        -- |                                                                        | -- 
        -- |                                                                        | -- 
        --  ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾  -- 


module Rasteriser
    where

import Prelude hiding (any, mapM_)
import SDL.Vect hiding (dot, normalize)
import SDL (($=))
import qualified SDL
import Debug.Trace as Trace
import Data.Word8
import qualified Data.Vector as V
import Data.Cross
import Camera
import Model
import SDLx
import Light
import Geometry
import Util
import Shader
import Data.Vec as Vec hiding (map, foldr)
import qualified Data.Vec as Vec  (map, foldr)
import Types
import Control.Monad.Par as Par

load_zbuffer :: Screen -> V.Vector (Double, Vec4 Word8)
load_zbuffer screen = V.fromList $ replicate ((width_i screen)*(height_i screen)) (0.0, toVec4 0 0 0 0)

load_shadowbuffer :: Screen -> V.Vector (Double, Vec4 Word8)
load_shadowbuffer screen = V.fromList $ replicate ((width_i screen)*(height_i screen)) (0.0, toVec4 255 255 255 255)

load_rasteriser :: Model -> Screen -> Camera -> Light -> Rasteriser
load_rasteriser  model screen camera light = Rasteriser zbuffer depthbuffer ambientbuffer model screen camera light 
                                        where zbuffer       = load_zbuffer screen
                                              depthbuffer   = load_shadowbuffer screen
                                              ambientbuffer = load_shadowbuffer screen

getBuffer :: Rasteriser -> Shader -> Buffer
getBuffer ras shader = case shader of   
                                (CameraShader {..})             ->  getZBuffer ras
                                (AmbientLightShader {..})       -> getAmbientBuffer ras
                                (DirectionalLightShader  {..})  -> getDepthBuffer ras      

process_triangle :: Rasteriser -> Shader -> Face  -> Par (Rasteriser, Shader)
process_triangle ras shader face  = do
                        let
                            ----------- VERTEX SHADER -----------
                            Face vertices uvs vertnorms = face

                            (screenVertices, shader') =  vertex_shade shader (ras) face 

                            -----------   SET BBOX -----------

                            fetchx i = getElem 0 (getElem i screenVertices )
                            fetchy i = getElem 1 (getElem i screenVertices )
                            fetchw i = getElem 3 (getElem i screenVertices )

                            bboxmin = foldr (\(x, y) (x', y') -> (max (0.0) (min x x'), max (0.0) (min y y')) )  
                                            (1000000.0, 1000000.0)
                                            [ (  (fetchx i)/(fetchw i) ,   (fetchy i)/(fetchw i)  ) |  i <- [0,1,2] ]

                            bboxmax = foldr (\(x, y) (x', y') -> ( min (screenWidth_d - 1)  (max x x'), min (screenHeight_d - 1) (max y y')) )  
                                            ((-1000000.0), (-1000000.0))   
                                            [ (  (fetchx i)/(fetchw i) ,   (fetchy i)/(fetchw i) ) |  i <- [0,1,2] ]
                        
                            --------------------------------------
                            

                        (updated_rasteriser, updated_shader) <-  draw_triangle ras shader' screenVertices   (floor $ fst bboxmin, floor $ fst bboxmax)   
                                                                                                            (floor $ snd bboxmin, floor $ snd bboxmax) 
                                                                                                            (floor $ fst bboxmin) 
                                                                                                            (floor $ snd bboxmin) 
                        return (updated_rasteriser, updated_shader)


-- #             Screen ->  Triangle Vertices   ->  Z-Buffer                     
draw_triangle :: Rasteriser -> Shader -> Vec3 (Vec4 Double) ->  (Int, Int) -> (Int, Int) -> Int -> Int ->  Par (Rasteriser, Shader)
draw_triangle ras shader screen_vertices (bbox_min_x, bbox_max_x) (bbox_min_y, bbox_max_y) px py =
    let recurse =  draw_triangle ras shader screen_vertices (bbox_min_x, bbox_max_x) (bbox_min_y, bbox_max_y) 
    in case () of 
      _ | (py > bbox_max_y)                      -> return (ras, shader)
        | (px > bbox_max_x)                      -> recurse bbox_min_x (py + 1)
        | otherwise -> do 
                let barycentric_inputs = fromVec3 $ Vec.map projectVec4to2D screen_vertices
                    ---
                    maybeBary = barycentric barycentric_inputs (toVec2 (to_double px) (to_double py) )

                case maybeBary of   Nothing   -> recurse (px + 1) py
                                    Just bary -> do
                                                let frag_depth = (Vec.getElem 2 $ multmv (getCurrentTri shader) bary) :: Double
                                                    pixelIndex = px + py * screenWidth_i
                                                if     fst (getBuffer ras shader V.! pixelIndex ) > frag_depth 
                                                then   recurse (px + 1) py
                                                else   do
                                                    (updated_ras , updated_shader) <- fragment_shade shader ras bary pixelIndex
                                                    draw_triangle updated_ras updated_shader screen_vertices (bbox_min_x, bbox_max_x) (bbox_min_y, bbox_max_y) (px+1) py
                                            
                                                          
      

render_screen :: Rasteriser -> Shader -> Shader -> Int -> Int -> IO ()
render_screen ras depthshader camerashader px py = do
                let screen = getScreen ras
                    index = px + py * screenWidth_i
                    pixel = V2  (screenWidth -  fromIntegral px) (screenHeight - fromIntegral py)
                    camrgba = snd $ getZBuffer ras V.! index 
                    depthrgba = snd $ getDepthBuffer ras V.! index
                sdl_put_pixel screen pixel $ vec4ToV4 $  camrgba 
                -- case shader of  (CameraShader {..}) -> let rgba = vec4ToV4 $ snd $ (getZBuffer ras) V.! index in sdl_put_pixel screen pixel (rgba) 
                --                 (DirectionalLightShader  {..}) -> let rgba = vec4ToV4 $ snd $ (getDepthBuffer ras) V.! index in sdl_put_pixel screen pixel (rgba) 
                --                 (AmbientLightShader  {..}) -> let amb = render_ambient px py (getAmbientBuffer ras)
                --                                               in case amb of Nothing -> return ()
                --                                                              Just rgba -> sdl_put_pixel screen pixel (vec4ToV4 rgba) 

                


render_ambient :: Int -> Int -> V.Vector (Double, Vec4 Word8) -> Maybe (Vec4 Word8)
render_ambient x y zbuffer = 
                     if     fst (zbuffer V.! (x + y * screenWidth_i)) > 0.0
                     then   let total   = foldr (\tote a -> tote + (m_PI/2) - max_elevation_angle zbuffer (Vec.map to_double $ toVec2 x y) (toVec2 (cos a) (sin a))   ) 0 [0, (m_PI/4) .. m_PI * 2 ]  
                                total'  = total/(m_PI*4)
                                total'' = (**) total' 100
                            in Just $ mult_rgba_d (toVec4 255 255 255 255) total''     
                     else   Nothing        
                                        
max_elevation_angle :: V.Vector (Double, Vec4 Word8) -> Vec2 Double -> Vec2 Double -> Double
max_elevation_angle zbuff p dir = 
    let (px, py) = fromVec2 p
        foo t maxangle =    if t > 100 
                            then maxangle
                            else let (curx, cury) = fromVec2 $ p + t * dir
                                 in  if (curx >= screenWidth_d || cury >= screenHeight_d || curx < 0 || cury < 0) 
                                     then maxangle
                                     else let distance = (Vec.norm (p - toVec2 curx cury)) :: Double
                                          in if distance >= 1.0 
                                             then foo  (t+1) maxangle
                                             else 
                                                let elevation = fst (zbuff V.! (floor curx + (floor cury) * screenWidth_i)) - fst (zbuff V.! (floor px + (floor py) * screenWidth_i))
                                                    maxangle' = max maxangle (atan (elevation/distance))
                                                in foo (t+1) maxangle'
    in foo 0 0







-- order_min_x :: (Vec3 Double, Vec3 Double) -> (Vec2 Double, Vec2 Double) -> ((Vec3 Double, Vec3 Double), (Vec2 Double, Vec2 Double))
-- order_min_x (Vec3 vAx vAy vAz, Vec3 vBx vBy vBz) (Vec2 vAu vAv, Vec2 vBu vBv)
--     | (vAx > vBx) = ((Vec3 vBx vBy vBz, Vec3 vAx vAy vAz), (Vec2 vBu vBv, Vec2 vAu vAv))
--     | otherwise   = ((Vec3 vAx vAy vAz, Vec3 vBx vBy vBz), (Vec2 vAu vAv, Vec2 vBu vBv))

-- order_vertices :: (Vec3 Double, Vec3 Double, Vec3 Double) -> (Vec2 Double, Vec2 Double, Vec2 Double) -> Int ->  ((Vec3 Double, Vec3 Double, Vec3 Double), (Vec2 Double, Vec2 Double, Vec2 Double))
-- order_vertices (Vec3 v0x v0y v0z, Vec3 v1x v1y v1z, Vec3 Vec2x Vec2y Vec2z)  (Vec2 v0u v0v, Vec2 v1u v1v, Vec2 Vec2u Vec2v) stage
--     | stage == 0 = if (v0y > v1y)   then order_vertices (Vec3 v1x v1y v1z, Vec3 v0x v0y v0z, Vec3 Vec2x Vec2y Vec2z)  (Vec2 v1u v1v, Vec2 v0u v0v, Vec2 Vec2u Vec2v) 1 
--                                     else order_vertices (Vec3 v0x v0y v0z, Vec3 v1x v1y v1z, Vec3 Vec2x Vec2y Vec2z)  (Vec2 v0u v0v, Vec2 v1u v1v, Vec2 Vec2u Vec2v) 1
--     | stage == 1 = if (v0y > Vec2y)   then order_vertices (Vec3 Vec2x Vec2y Vec2z, Vec3 v1x v1y v1z, Vec3 v0x v0y v0z)  (Vec2 Vec2u Vec2v, Vec2 v1u v1v, Vec2 v0u v0v) 2
--                                     else order_vertices (Vec3 v0x v0y v0z, Vec3 v1x v1y v1z, Vec3 Vec2x Vec2y Vec2z)  (Vec2 v0u v0v, Vec2 v1u v1v, Vec2 Vec2u Vec2v) 2
--     | stage == 2 = if (v1y > Vec2y)   then order_vertices (Vec3 v0x v0y v0z, Vec3 Vec2x Vec2y Vec2z, Vec3 v1x v1y v1z)  (Vec2 v0u v0v, Vec2 Vec2u Vec2v, Vec2 v1u v1v) 3
--                                     else order_vertices (Vec3 v0x v0y v0z, Vec3 v1x v1y v1z, Vec3 Vec2x Vec2y Vec2z)  (Vec2 v0u v0v, Vec2 v1u v1v, Vec2 Vec2u Vec2v) 3
--     | otherwise = ((Vec3 v0x v0y v0z, Vec3 v1x v1y v1z, Vec3 Vec2x Vec2y Vec2z), (Vec2 v0u v0v, Vec2 v1u v1v, Vec2 Vec2u Vec2v))

-- order_min_x_i :: (Vec3 Int, Vec3 Int) -> (Vec2 Int, Vec2 Int) -> ((Vec3 Int, Vec3 Int), (Vec2 Int, Vec2 Int))
-- order_min_x_i (Vec3 vAx vAy vAz, Vec3 vBx vBy vBz) (Vec2 vAu vAv, Vec2 vBu vBv)
--     | (vAx > vBx) = ((Vec3 vBx vBy vBz, Vec3 vAx vAy vAz), (Vec2 vBu vBv, Vec2 vAu vAv))
--     | otherwise   = ((Vec3 vAx vAy vAz, Vec3 vBx vBy vBz), (Vec2 vAu vAv, Vec2 vBu vBv))

-- order_vertices_i :: Vec3 (Vec3 Int) -> Vec3 (Vec2 Int) -> Int -> (Vec3 (Vec3 Int), Vec3 (Vec2 Int))
-- order_vertices_i (Vec3 (Vec3 v0x v0y v0z) (Vec3 v1x v1y v1z) (Vec3 Vec2x Vec2y Vec2z))  (Vec3 (Vec2 v0u v0v) (Vec2 v1u v1v) (Vec2 Vec2u Vec2v)) stage
--     | stage == 0 = if (v0y > v1y)   then order_vertices_i (Vec3 (Vec3 v1x v1y v1z) (Vec3 v0x v0y v0z) (Vec3 Vec2x Vec2y Vec2z)) (Vec3 (Vec2 v1u v1v) (Vec2 v0u v0v) (Vec2 Vec2u Vec2v)) 1 
--                                     else order_vertices_i (Vec3 (Vec3 v0x v0y v0z) (Vec3 v1x v1y v1z) (Vec3 Vec2x Vec2y Vec2z)) (Vec3 (Vec2 v0u v0v) (Vec2 v1u v1v) (Vec2 Vec2u Vec2v)) 1
--     | stage == 1 = if (v0y > Vec2y)   then order_vertices_i (Vec3 (Vec3 Vec2x Vec2y Vec2z) (Vec3 v1x v1y v1z) (Vec3 v0x v0y v0z)) (Vec3 (Vec2 Vec2u Vec2v) (Vec2 v1u v1v) (Vec2 v0u v0v)) 2
--                                     else order_vertices_i (Vec3 (Vec3 v0x v0y v0z) (Vec3 v1x v1y v1z) (Vec3 Vec2x Vec2y Vec2z)) (Vec3 (Vec2 v0u v0v) (Vec2 v1u v1v) (Vec2 Vec2u Vec2v)) 2
--     | stage == 2 = if (v1y > Vec2y)   then order_vertices_i (Vec3 (Vec3 v0x v0y v0z) (Vec3 Vec2x Vec2y Vec2z) (Vec3 v1x v1y v1z)) (Vec3 (Vec2 v0u v0v) (Vec2 Vec2u Vec2v) (Vec2 v1u v1v)) 3
--                                     else order_vertices_i (Vec3 (Vec3 v0x v0y v0z) (Vec3 v1x v1y v1z) (Vec3 Vec2x Vec2y Vec2z)) (Vec3 (Vec2 v0u v0v) (Vec2 v1u v1v) (Vec2 Vec2u Vec2v)) 3
--     | otherwise = (Vec3 (Vec3 v0x v0y v0z) (Vec3 v1x v1y v1z) (Vec3 Vec2x Vec2y Vec2z), Vec3 (Vec2 v0u v0v)(Vec2 v1u v1v)(Vec2 Vec2u Vec2v))
                                                                            