{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Graphics 
  ( GLContext(..)
  , loadTexture
  , drawArray
  , drawJsArray
  , ortho2D
  , initGL
  , setColor
  , DrawStack
  , drawStatic
  , Graphics.Texture(..)
  ) where 

import Control.Monad (forM_, unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (State, execState)
import Control.Monad.State.Class (modify)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Reader (ReaderT, ask)
import Control.Monad.Trans.State.Strict (StateT)

import Data.Coerce (Coercible, coerce)

import Drawing (Color(..), MonadDraw(..), MonadCanvas(..), Texture(..), Rect(..), rectLeft, rectRight, rectUp, rectDown)

import GHC.JS.Prim (JSVal)
import JS (Float32Array, HTMLImageElement, toJSString, toFloat32Array, consoleLog, fromJSString)
import WebGL

foreign import javascript "(() => gl.drawingBufferWidth)"
    js_gl_width :: IO Int

foreign import javascript "(() => gl.drawingBufferHeight)"
    js_gl_height :: IO Int

foreign import javascript "(($1) => $1.width)"
    js_image_width :: HTMLImageElement -> IO Int

foreign import javascript "(($1) => $1.height)"
    js_image_height :: HTMLImageElement -> IO Int

vertexShaderSource :: String
vertexShaderSource = 
    " // VERTEX SHADER                                \n\
    \ attribute vec4 position;                        \n\
    \ attribute vec4 color;                           \n\
    \ attribute vec4 mix;                             \n\
    \ uniform vec4 transform;                         \n\
    \ varying vec2 frag_coord;                        \n\
    \ varying vec4 frag_color;                        \n\
    \ varying vec4 frag_mix;                          \n\
    \ void main() {                                   \n\
    \     gl_Position = vec4(                         \n\
    \         transform.x * position.x + transform.z, \n\
    \         transform.y * position.y + transform.w, \n\
    \         0.0,                                    \n\
    \         1.0                                     \n\
    \     );                                          \n\
    \     frag_coord = vec2(position.z, position.w);  \n\
    \     frag_color = color;                         \n\
    \     frag_mix   = mix;                           \n\
    \ }                                                 "

fragmentShaderSource :: String
fragmentShaderSource = 
    " // FRAGMENT SHADER                                       \n\
    \ precision mediump float;                                 \n\
    \ varying vec2 frag_coord;                                 \n\
    \ varying vec4 frag_color;                                 \n\
    \ varying vec4 frag_mix;                                   \n\
    \ uniform vec4 ucolor;                                     \n\
    \ uniform sampler2D sampler0;                              \n\
    \ uniform sampler2D sampler1;                              \n\
    \ void main() {                                            \n\
    \     gl_FragColor = frag_mix.x * frag_color;              \n\
    \     gl_FragColor +=                                      \n\
    \           frag_mix.y * texture2D(sampler0, frag_coord);  \n\
    \     gl_FragColor +=                                      \n\
    \           frag_mix.z * texture2D(sampler1, frag_coord);  \n\
    \     gl_FragColor.x *= ucolor.x;                          \n\
    \     gl_FragColor.y *= ucolor.y;                          \n\
    \     gl_FragColor.z *= ucolor.z;                          \n\
    \     gl_FragColor.w *= ucolor.w;                          \n\
    \ }"

data Texture = Texture
  { texWebGLTexture :: WebGLTexture
  , texWidth        :: Int 
  , texHeight       :: Int 
  , texUnit         :: Int
  }

blendFactors :: Graphics.Texture -> (Float, Float, Float, Float)
blendFactors texture =
    case texUnit texture of 
        0 -> (0, 1, 0, 0)
        1 -> (0, 0, 1, 0)
        2 -> (0, 0, 0, 1)
        _ -> (0, 0, 0, 0) 

data GLContext = GLContext
 { glRenderingContext :: !WebGLRenderingContext
 , glProgram          :: !WebGLProgram
 , glDrawBuffer       :: !WebGLBuffer
 , glTransformUniform :: !WebGLUniformLocation
 , glColorUniform     :: !WebGLUniformLocation
 }
 

foreign import javascript "(($1) => Boolean($1))"
    js_Boolean :: JSVal -> Bool

loadShader :: WebGLRenderingContext -> GLenum -> String -> IO (WebGLShader)
loadShader gl _type source = do
    shader <- createShader gl _type
    shaderSource gl shader (toJSString source)
    compileShader gl shader
    status <- getShaderParameter gl shader gl_COMPILE_STATUS
    unless (js_Boolean status) $ do
        message <- fromJSString <$> getShaderInfoLog gl shader
        fail $ "Failed to compile shader:\n" ++ message
    return shader

loadProgram :: WebGLRenderingContext -> [WebGLShader] -> IO (WebGLProgram)
loadProgram gl shaders = do
    program <- createProgram gl
    forM_ shaders $ attachShader gl program
    linkProgram gl program
    status <- getProgramParameter gl program gl_LINK_STATUS
    unless (js_Boolean status) $ do
        message <- fromJSString <$> getProgramInfoLog gl program
        fail $ "Failed to link program:\n" ++ message
    return program

foreign import javascript "(($1, $2) => $1.texImage2D($1.TEXTURE_2D, 0, $1.RGBA, $1.RGBA, $1.UNSIGNED_BYTE, $2))"
    js_texImage2D :: WebGLRenderingContext -> HTMLImageElement -> IO ()

loadTexture :: WebGLRenderingContext -> Int -> HTMLImageElement -> IO Graphics.Texture
loadTexture gl unit image = do
    width  <- js_image_width image
    height <- js_image_height image
    texture <- createTexture gl 
    activeTexture gl (gl_TEXTURE0 + unit)
    bindTexture gl gl_TEXTURE_2D texture
    -- pixelStorei gl gl_UNPACK_FLIP_Y_WEBGL 1
    pixelStorei gl gl_UNPACK_PREMULTIPLY_ALPHA_WEBGL 1
    js_texImage2D gl image
    texParameteri gl gl_TEXTURE_2D gl_TEXTURE_WRAP_S gl_CLAMP_TO_EDGE 
    texParameteri gl gl_TEXTURE_2D gl_TEXTURE_WRAP_T gl_CLAMP_TO_EDGE 
    texParameteri gl gl_TEXTURE_2D gl_TEXTURE_MIN_FILTER gl_NEAREST
    texParameteri gl gl_TEXTURE_2D gl_TEXTURE_MAG_FILTER gl_NEAREST
    generateMipmap gl gl_TEXTURE_2D
    return $ 
        Texture texture width height unit

foreign import javascript "(($1, $2, $3, $4) => $1.bufferData($2, $3, $4))"
    js_bufferData :: WebGLRenderingContext -> GLenum -> Float32Array -> GLenum -> IO ()

drawJsArray :: GLContext -> GLenum -> Float32Array -> IO ()
drawJsArray context shape array = do
    bindBuffer gl gl_ARRAY_BUFFER (glDrawBuffer context)
    js_bufferData gl gl_ARRAY_BUFFER array gl_STATIC_DRAW
    n <- js_length array
    drawArrays gl shape 0 $ n `div` 12
  where
    gl = glRenderingContext context

drawArray :: GLContext -> GLenum -> [Float] -> IO ()
drawArray context shape buffer = do
    array <- toFloat32Array buffer
    drawJsArray context shape array

ortho2D :: GLContext -> Float -> Float -> Float -> Float -> IO ()
ortho2D context left right bottom top =
    uniform4f 
        (glRenderingContext context) (glTransformUniform context) sx sy tx ty
  where 
    sx = 2 / (right - left)
    sy = 2 / (top - bottom)
    tx = (right + left) / (left - right)
    ty = (top + bottom) / (bottom - top)

setColor :: GLContext -> Color -> IO ()
setColor context (Color r g b a) =
    uniform4f gl location r g b a
  where
    gl = glRenderingContext context
    location = glColorUniform context


initGL :: WebGLRenderingContext -> IO GLContext
initGL gl = do
    -- initilaize textures 
    activeTexture gl gl_TEXTURE0
    enable gl gl_BLEND
    blendFunc gl gl_SRC_COLOR gl_ONE_MINUS_SRC_ALPHA
    -- compile shaders
    vertexShader   <- loadShader gl gl_VERTEX_SHADER vertexShaderSource
    fragmentShader <- loadShader gl gl_FRAGMENT_SHADER fragmentShaderSource
    program <- loadProgram gl [vertexShader, fragmentShader]
    -- initialized shaders
    useProgram gl program
    buffer <- createBuffer gl
    bindBuffer gl gl_ARRAY_BUFFER buffer
    --    position attrib
    positionAttrib <- getAttribLocation gl program (toJSString "position")
    vertexAttribPointer gl positionAttrib 4 gl_FLOAT False (12 * 4) 0
    enableVertexAttribArray gl positionAttrib
    --    color attrib
    colorAttrib <- getAttribLocation gl program (toJSString "color")
    vertexAttribPointer gl colorAttrib 4 gl_FLOAT False (12 * 4) (4 * 4)
    enableVertexAttribArray gl colorAttrib
    --    mix attrib
    mixAttrib <- getAttribLocation gl program (toJSString "mix")
    vertexAttribPointer gl mixAttrib 4 gl_FLOAT False (12 * 4) (8 * 4)
    enableVertexAttribArray gl mixAttrib
    --    transform uniform
    transformUniform <- getUniformLocation gl program (toJSString "transform")
    uniform4f gl transformUniform 0 0 0 0
    --    color uniform
    colorUniform <- getUniformLocation gl program (toJSString "ucolor")
    uniform4f gl colorUniform 1 1 1 1
    --    sampler uniform
    samplerUniform <- getUniformLocation gl program (toJSString "sampler0")
    uniform1i gl samplerUniform 0
    samplerUniform <- getUniformLocation gl program (toJSString "sampler1")
    uniform1i gl samplerUniform 1
    -- finish
    return $ GLContext gl program buffer transformUniform colorUniform
    
    

type DrawStack = StateT Float32Array (ReaderT GLContext IO)

foreign import javascript "($1 => $1.length)"
    js_length :: Float32Array -> IO Int

foreign import javascript "(($1, $2, $3) => {$1[$2] = $3})"
    js_assign :: Float32Array -> Int -> Float -> IO ()

foreign import javascript "(($1, $2) => new Float32Array(($1.buffer.byteLength >= 4 * $2) ? $1.buffer : $1.buffer.transfer(Math.max(4 * $2, $1.buffer.byteLength * 2)), 0, $2))"
    js_expand :: Float32Array -> Int -> IO Float32Array



type StaticDraw = State [[Float]]

drawVertex :: (Float, Float) -> (Float, Float) -> Color -> (Float, Float, Float, Float) -> StaticDraw ()
drawVertex (x, y) (u, v) (Color r g b a) (m1, m2, m3, m4) = do 
    modify ([x, y, u, v, r, g, b, a, m1, m2, m3, m4] :)
    -- array <- get
    -- i <- liftIO $ js_length array
    -- array <- liftIO $ js_expand array (i + 8)
    -- liftIO $ js_assign array (i + 0) x
    -- liftIO $ js_assign array (i + 1) y
    -- liftIO $ js_assign array (i + 2) u 
    -- liftIO $ js_assign array (i + 3) v
    -- liftIO $ js_assign array (i + 4) r
    -- liftIO $ js_assign array (i + 5) g
    -- liftIO $ js_assign array (i + 6) b
    -- liftIO $ js_assign array (i + 7) t
    -- put array
    -- liftIO $ js_console_log (coerce array)

drawStatic :: StaticDraw a -> IO Float32Array
drawStatic = toFloat32Array . concat . reverse . flip execState []

instance MonadDraw StaticDraw where

    drawRect (Rect x y width height) color = do
        drawVertex (x + width, y + height) (0, 0) color (1, 0, 0, 0)
        drawVertex (x,         y + height) (0, 0) color (1, 0, 0, 0)
        drawVertex (x + width, y         ) (0, 0) color (1, 0, 0, 0)
        drawVertex (x,         y         ) (0, 0) color (1, 0, 0, 0)
        drawVertex (x + width, y         ) (0, 0) color (1, 0, 0, 0)
        drawVertex (x,         y + height) (0, 0) color (1, 0, 0, 0)

instance Drawing.Texture StaticDraw Graphics.Texture where

    drawTexture texture (Rect x y width height) (Rect x' y' width' height') = do
        drawVertex (x + width, y + height)  (x' + width', y'          ) (Color 1 1 1 1) (blendFactors texture)
        drawVertex (x,         y + height)  (x',          y'          ) (Color 1 1 1 1) (blendFactors texture)
        drawVertex (x + width, y         )  (x' + width', y' + height') (Color 1 1 1 1) (blendFactors texture)
        drawVertex (x,         y         )  (x',          y' + height') (Color 1 1 1 1) (blendFactors texture)
        drawVertex (x + width, y         )  (x' + width', y' + height') (Color 1 1 1 1) (blendFactors texture)
        drawVertex (x,         y + height)  (x',          y'          ) (Color 1 1 1 1) (blendFactors texture)



instance MonadDraw DrawStack where

    drawRect (Rect x y width height) (Color r g b a) = do
        context <- lift ask
        liftIO $ drawArray context 
            gl_TRIANGLE_STRIP
            [x + width, y + height, 0, 0, r, g, b, a, 1, 0, 0, 0,
             x,         y + height, 0, 0, r, g, b, a, 1, 0, 0, 0,
             x + width, y         , 0, 0, r, g, b, a, 1, 0, 0, 0,
             x,         y         , 0, 0, r, g, b, a, 1, 0, 0, 0]

instance Drawing.Texture DrawStack Graphics.Texture where 

    drawTexture texture (Rect x y width height) (Rect x' y' width' height') = do
        context <- lift ask
        liftIO $ drawArray context 
            gl_TRIANGLE_STRIP
            [x + width, y + height, x' + width', y'          , 1, 1, 1, 1, m1, m2, m3, m4,
             x,         y + height, x',          y'          , 1, 1, 1, 1, m1, m2, m3, m4,
             x + width, y         , x' + width', y' + height', 1, 1, 1, 1, m1, m2, m3, m4,
             x,         y         , x',          y' + height', 1, 1, 1, 1, m1, m2, m3, m4]
      where 
        (m1, m2, m3, m4) = blendFactors texture

instance MonadCanvas DrawStack where

    canvasWidth  = fromIntegral <$> liftIO js_gl_width
    canvasHeight = fromIntegral <$> liftIO js_gl_height

    setCoordinates area coords = do
        context <- lift ask
        liftIO $ do 
            ortho2D 
                context 
                (rectLeft coords) (rectRight coords) 
                (rectDown coords) (rectUp coords)
            viewport 
                (glRenderingContext context)
                (round $ rectX area)
                (round $ rectY area)
                (round $ rectWidth area)
                (round $ rectHeight area)

    clearCanvas (Color r g b a)= do
        gl <- glRenderingContext <$> lift ask
        liftIO $ clearColor gl r g b a
        liftIO $ clear gl gl_COLOR_BUFFER_BIT