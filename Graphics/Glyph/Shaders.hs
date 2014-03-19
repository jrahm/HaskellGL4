module Graphics.Glyph.Shaders where

import Graphics.Rendering.OpenGL
import qualified Data.ByteString as BS
import Control.Monad
import Data.Maybe

loadShader :: ShaderType -> FilePath -> IO (String, Maybe Shader)
loadShader typ path = do
    shader <- createShader typ
    ( shaderSourceBS shader $= ) =<< BS.readFile path
    compileShader shader

    ok <- get (compileStatus shader)
    infoLog <- get (shaderInfoLog shader)

    unless ok $
        deleteObjectNames [shader]

    return ( infoLog, if not ok then Nothing else Just shader );


loadShaders :: [(ShaderType,FilePath)] -> IO [(String, Maybe Shader)]
loadShaders = mapM ( uncurry loadShader )

workingShaders :: [(a, Maybe Shader)] -> [Shader]
workingShaders lst = map (fromJust . snd) (filter (isJust . snd) lst)

createShaderProgram :: [Shader] -> IO (String, Maybe Program)
createShaderProgram shaders = do
    p <- createProgram
    mapM_ (attachShader p) shaders
    linkProgram p

    ok <- get $ linkStatus p
    info <- get $ programInfoLog p

    unless ok $
        deleteObjectNames [p]

    return ( info, if not ok then Nothing else Just p )

getUniform :: Uniform a => String -> IO (Maybe (StateVar a))
getUniform name =
    get currentProgram >>= (\pr -> case pr of
            Just p -> liftM (Just . uniform) (get $ uniformLocation p name)
            Nothing -> return Nothing )

getUniformForProgram :: Uniform a => String -> Program -> IO (StateVar a)
getUniformForProgram name prog =
    liftM uniform (get $ uniformLocation prog name)
    

getUniformLocation :: String -> IO (Maybe UniformLocation)
getUniformLocation name =
    get currentProgram >>= maybe (return Nothing) (\prog ->
        liftM Just (get $ uniformLocation prog name) )
