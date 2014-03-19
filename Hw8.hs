{-# LANGUAGE TemplateHaskell #-}
module Main where 

import Control.Applicative
import Control.Monad

import Data.Setters
import Data.Maybe
import Data.Word

import Debug.Trace

import Graphics.Rendering.OpenGL
import Graphics.Rendering.OpenGL.Raw.Core31
import Graphics.UI.SDL as SDL
import Graphics.Glyph.GLMath
import Graphics.Glyph.Mat4

import Graphics.UI.SDL.Image
import Graphics.Glyph.Textures
import Graphics.Glyph.Shaders
import Graphics.Glyph.Util
import Graphics.Glyph.BufferBuilder

import Control.DeepSeq
import System.Exit
import System.Random

import Debug.Trace
import Foreign.Storable
import Foreign.Ptr

class Drawable a where
    -- mvMat -> pMat -> obj -> IO ()
    draw :: a -> IO ()

data GlyphObject a = GlyphObject {
    bufferObject :: BufferObject, -- buffer
    compiledData :: (CompiledBuild GLfloat), -- compiled data
    vertexAttribute :: AttribLocation, -- vertex attribute
    normalAttribute :: (Maybe AttribLocation), -- normal attrib
    colorAttribute  :: (Maybe AttribLocation), -- color attrib
    textureAttribute :: (Maybe AttribLocation), -- texture attrib
    resources :: a, -- Resources
    setupRoutine :: (Maybe (GlyphObject a -> IO ())), -- Setup
    teardownRoutine :: (Maybe (GlyphObject a -> IO ())) -- Tear down
}

$(declareSetters ''GlyphObject)
makeGlyphObject :: Builder GLfloat x ->
    AttribLocation ->
    Maybe AttribLocation ->
    Maybe AttribLocation ->
    Maybe AttribLocation ->
    a ->
    Maybe (GlyphObject a -> IO ()) ->
    Maybe (GlyphObject a -> IO ()) ->
    IO (GlyphObject a)

makeGlyphObject builder vertAttr normAttr colorAttr textureAttr res setup tear = do
    compiled <- compilingBuilder builder
    buffer <- createBufferObject ArrayBuffer compiled
    return $ GlyphObject buffer compiled vertAttr normAttr colorAttr textureAttr res setup tear

glyphObjectPrepare :: GlyphObject a -> (GlyphObject a -> IO()) -> GlyphObject a
glyphObjectPrepare (GlyphObject a b c d e f g _ i) h = GlyphObject a b c d e f g (Just h) i

glyphObjectTeardown :: GlyphObject a -> (GlyphObject a -> IO()) -> GlyphObject a
glyphObjectTeardown (GlyphObject a b c d e f g h _) i = GlyphObject a b c d e f g h (Just i)

instance (Show a) => Show (GlyphObject a) where
    show (GlyphObject _ co vAttr nAttr cAttr tAttr res _ _) =
        "[GlyphObject compiled=" ++! co ++ " vertAttr=" ++! vAttr ++
            " normalAttr="++!nAttr++" colorAttr="++!cAttr++" textureAttr="++!tAttr++" res="++!res++"]"

instance Drawable (GlyphObject a) where
    draw obj@(GlyphObject bo co vAttr nAttr cAttr tAttr _ setup tearDown) = do
        {- Setup whatever we need for the object to draw itself -}
        maybe (return ()) (apply obj) setup

        {- Get the array descriptors for the possible
         - parts -}
        let vad = vertexArrayDescriptor co
        let nad = normalArrayDescriptor co
        let cad = colorArrayDescriptor co
        let tad = textureArrayDescriptor co

        bindBuffer ArrayBuffer $= Just bo
        let enabled = catMaybes $
             map liftMaybe [(Just vAttr,Just vad), (nAttr, nad), (cAttr,cad), (tAttr,tad)]

        forM_ enabled $ \(attr, ad) -> do
            vertexAttribPointer attr $= (ToFloat, ad)
            vertexAttribArray attr $= Enabled

        drawArrays Quads 0 (bufferLength co)
            
        forM_ enabled $ \(attr, _) -> do
            vertexAttribArray attr $= Disabled
            
        {- Tear down whatever the object needs -}
        maybe (return ()) (apply obj) tearDown
        where liftMaybe t@(Just a, Just b) = Just (a,b)
              liftMaybe _ = Nothing
              apply obj f = f obj

data Uniforms = Uniforms {
    dxU :: UniformLocation,
    dyU :: UniformLocation,

    textureU :: UniformLocation,
    earthU :: UniformLocation,
    cloudsU :: UniformLocation,

    timeU :: UniformLocation,
    lightsU :: UniformLocation,

    randomU :: UniformLocation,
    winterU :: UniformLocation
} deriving Show

data TextureData = TextureData {
    textureSize :: (Int,Int),
    textureObject :: TextureObject } deriving Show
 
data Resources = Resources {
    object :: GlyphObject Uniforms,
    backDrop :: GlyphObject (Program,UniformLocation),
    moon :: GlyphObject (Program,
                UniformLocation,
                UniformLocation,
                UniformLocation,
                UniformLocation,
                UniformLocation,
                UniformLocation,
                UniformLocation),
    resTexture :: TextureData,
    earthTex :: TextureData,
    cloudsTex :: TextureData,
    lightsTex :: TextureData,
    winterTex :: TextureData,
    spaceTex :: TextureData,
    moonTex :: TextureData,
    program :: Program,
    lightU :: UniformLocation,
    pU :: UniformLocation,
    mvU :: UniformLocation,
    normalMatU :: UniformLocation,
    resTime :: GLfloat,
    pMatrix :: Mat4 GLfloat,
    eyeLocation :: (GLfloat,GLfloat,GLfloat),
    difEyeLocation :: (GLfloat, GLfloat, GLfloat),
    lightPos :: (GLfloat,GLfloat,GLfloat),
    useNoise :: Bool,
    dTime :: GLfloat
} deriving (Show)

$(declareSetters ''Resources)

makeTexture :: IO TextureObject
makeTexture = do
    texobj <- liftM head $ genObjectNames 1
    textureBinding Texture2D $= Just texobj
    textureFilter Texture2D $= ((Linear', Nothing), Linear')
    return texobj

enumEq :: Enum a => a -> a -> Bool
enumEq a = (fromEnum a ==) . fromEnum

enumNeq :: Enum a => a -> a -> Bool
enumNeq a = not . enumEq a

loadProgram :: String -> String -> IO Program
loadProgram vert frag = do
    shaders <- loadShaders [
        (VertexShader, vert),
        (FragmentShader, frag) ]
    mapM_ (putStrLn . fst) shaders
    (linklog, maybeProg) <- createShaderProgram (workingShaders shaders)

    when (isNothing maybeProg) $ do
        putStrLn "Failed to link program"
        putStrLn linklog
        exitWith (ExitFailure 111)

    (return . fromJust)  maybeProg

loadBackdropProgram :: IO Program
loadBackdropProgram = do
    shaders <- loadShaders [
        (VertexShader, "shaders/space.vert"),
        (FragmentShader, "shaders/space.frag") ]
    mapM_ (putStrLn . fst) shaders
    (linklog, maybeProg) <- createShaderProgram (workingShaders shaders)

    when (isNothing maybeProg) $ do
        putStrLn "Failed to link program"
        putStrLn linklog
        exitWith (ExitFailure 111)

    (return . fromJust)  maybeProg

quad :: Builder GLfloat ()
quad = do
    forM_ [
            (-1,-1,0.0),
            (-1, 1,0.0),
            ( 1, 1,0.0),
            ( 1,-1,0.0)
        ] $ \(a,b,c) -> do
              bVertex3 (a,b,c)

circle :: GLfloat -> GLfloat -> Builder GLfloat ()
circle r step = do
    let lst = concat [[(r,th-step,ph-step),
                       (r,th+step,ph-step),
                       (r,th+step,ph+step),
                       (r,th-step,ph+step)]
                       | th <- [0,step..359-step],
                         ph <- [-90,-90+step..89-step]]
    mapM_ ( doUv >&> ((bNormal3 >&> bVertex3) . toEuclidian) ) lst
    where doUv (_,th,ph) = bTexture2 (1 - th / 360.0, 1 - (ph / 180.0 + 0.5))



makeResources :: IO Resources
makeResources =
        let pMatrix' = perspectiveMatrix 50 1.8 0.1 100 in
        loadProgram "shaders/normal.vert" "shaders/textured.frag" >>= (\prog -> do
            glo <- makeGlyphObject
                    <$> (pure $ circle 1 3)
                    <*> get (attribLocation prog "in_position")
                    <*> (get (attribLocation prog "in_normal") >>= return . Just)
                    <*> pure Nothing
                    <*> (get (attribLocation prog "in_texMapping") >>= return . Just)
                    <*> do Uniforms
                            <$> get (uniformLocation prog "dX")
                            <*> get (uniformLocation prog "dY")
                            <*> get (uniformLocation prog "texture")
                            <*> get (uniformLocation prog "earth")
                            <*> get (uniformLocation prog "clouds")
                            <*> get (uniformLocation prog "time")
                            <*> get (uniformLocation prog "lights")
                            <*> get (uniformLocation prog "random")
                            <*> get (uniformLocation prog "winter")
                    <*> pure Nothing
                    <*> pure Nothing
                    
            prog2 <- loadBackdropProgram
            backDrop <- makeGlyphObject
                            <$> pure quad
                            <*> get (attribLocation prog "in_position")
                            <*> pure Nothing
                            <*> pure Nothing
                            <*> pure Nothing
                            <*> (get (uniformLocation prog2 "texture") >>= \x-> return (prog2,x))
                            <*> pure Nothing
                            <*> pure Nothing

            moonProg <- loadProgram "shaders/moon.vert" "shaders/moon.frag"
            moon <- makeGlyphObject
                    <$> pure (circle 0.2 5)
                    <*> get (attribLocation moonProg "in_position")
                    <*> liftM Just (get (attribLocation moonProg "in_normal"))
                    <*> pure Nothing
                    <*> liftM Just (get (attribLocation moonProg "in_texMapping"))
                    <*> do (,,,,,,,)
                            <$> pure moonProg
                            <*> get (uniformLocation moonProg "texture")
                            <*> get (uniformLocation moonProg "lightPos")
                            <*> get (uniformLocation moonProg "mvMat")
                            <*> get (uniformLocation moonProg "pMat")
                            <*> get (uniformLocation moonProg "time")
                            <*> get (uniformLocation moonProg "dX")
                            <*> get (uniformLocation moonProg "dY")
                    <*> pure Nothing
                    <*> pure Nothing

            Resources
                <$> glo
                <*> backDrop
                <*> moon
                <*> (makeTexture >>= genRandomTexture)
                <*> (load ("textures/earth.png") >>= textureFromSurface)
                <*> (load ("textures/clouds.png") >>= textureFromSurface)
                <*> (load ("textures/lights.png") >>= textureFromSurface)
                <*> (load ("textures/winter.png") >>= textureFromSurface)
                <*> (load ("textures/space.png") >>= textureFromSurface)
                <*> (load ("textures/moon.png") >>= textureFromSurface)
                <*> pure prog
                <*> get (uniformLocation prog "light")
                <*> get (uniformLocation prog "pMat")
                <*> get (uniformLocation prog "mvMat")
                <*> get (uniformLocation prog "normalMat")
                <*> pure 0
                <*> pure pMatrix'
                <*> pure (5,45.1,0.1)
                <*> pure (0,0,0)
                <*> pure (20,0.1,0.1)
                <*> pure False
                <*> pure 1.0
            )
        
printErrors :: String -> IO ()
printErrors ctx =
    get errors >>= mapM_ (putStrLn . (("GL["++ctx++"]: ")++) . show)

setupMvp :: Mat4 GLfloat ->Resources -> IO ()
setupMvp mvMatrix res =
    do
--        putStrLn ("lookAt: " ++! (Vec3 . toEuclidian $ eyeLocation res) ++ " "
--                    ++! (Vec3 (0,0,0)) ++ " " ++! (Vec3 (0,1,0)))
--        print mvMatrix
        _ <- (uniform (pU res) $= pMatrix res)
        t <- (uniform (mvU res) $= mvMatrix)
        return t

setupLighting :: Mat4 GLfloat -> Resources -> UniformLocation -> IO ()
setupLighting mvMat res lu =
    let (+++) = zipWithT3 (+)
        (a,b,c) = (toEuclidian $ lightPos res)
        Vec4 (x,y,z,_) = mvMat `glslMatMul` Vec4 (a,b,c,1)
        normalMat = toNormalMatrix mvMat
    in do
      --  putStrLn $ "Multiply "++!(a,b,c)++" by\n"++!mvMat++"\nyeilds "++!(x,y,z)
        uniform lu $= (Vertex3 x y z)
        case normalMat of
            Just mat -> uniform (normalMatU res) $= mat
            _ -> putStrLn "Normal matrix could not be computed"


setupTexturing :: TextureData -> UniformLocation -> Int -> IO ()
setupTexturing (TextureData _ to) tu unit = do
    texture Texture2D $= Enabled
    activeTexture $= TextureUnit (fromIntegral unit)
    textureBinding Texture2D $= Just to
    uniform tu $= Index1 (fromIntegral unit::GLint)
    printErrors "setupTexturing"


display :: SDL.Surface -> Resources -> IO Resources
display surf res = do
    clear [ColorBuffer, DepthBuffer]
    clearColor $= Color4 0.3 0.3 0.3 1.0
    SDL.flip surf

    depthFunc $= Nothing
    draw $ glyphObjectPrepare (backDrop res) $ \obj -> do
                let (prg,uni) = (resources obj)
                currentProgram $= Just prg
                setupTexturing (spaceTex res) uni 0

    currentProgram $= Just (program res)
    let (_,_,ph) = eyeLocation res
    let up = if ph' >= 90 && ph' < 270 then Vec3 (0,-1,0) else Vec3 (0,1,0)
            where ph' = (floor ph::Int) `mod` 360
    let mvMatrix = lookAtMatrix (Vec3 . toEuclidian $ eyeLocation res) (Vec3 (0,0,0)) up
    
    vertexProgramPointSize $= Enabled
    draw $ glyphObjectPrepare (object res) $ \glo -> do
                depthFunc $= Just Less
                let bumpMap = if useNoise res then resTexture else earthTex
                let uniforms = resources glo
                let (w,h) = mapT2 fromIntegral (textureSize $ bumpMap res)
                uniform (dxU uniforms) $= Index1 (1.0/w::GLfloat)
                uniform (dyU uniforms) $= Index1 (1.0/h::GLfloat)
                uniform (timeU uniforms) $= Index1 (resTime res)
                setupMvp mvMatrix res
                setupLighting  mvMatrix res (lightU res)
                setupTexturing (bumpMap res) (textureU uniforms) 0
                setupTexturing (earthTex res) (earthU uniforms) 1
                setupTexturing (cloudsTex res) (cloudsU uniforms) 2
                setupTexturing (lightsTex res) (lightsU uniforms) 3
                setupTexturing (resTexture res) (randomU uniforms) 4
                setupTexturing (winterTex res) (winterU uniforms) 5

    draw $ glyphObjectPrepare (moon res) $ \glo -> do
            let (prog, texU, lU, mvMatU, pMatU, timeUn,dxUn,dyUn) = resources glo
            let (w,h) = mapT2 fromIntegral (textureSize $ moonTex res)
            let time = resTime res
            currentProgram $= Just prog
            uniform mvMatU $= (mvMatrix ==> Vec4 (10*gsin (time / 10),0,10*gcos (time / 10),0))
            uniform pMatU $= (pMatrix res)
            uniform timeUn $= Index1 time
            uniform dxUn $= Index1 (1.0/w::GLfloat)
            uniform dyUn $= Index1 (1.0/w::GLfloat)
            setupTexturing (moonTex res) texU 0
            setupLighting mvMatrix res lU

    SDL.glSwapBuffers
    return res

digestEvents :: Resources -> IO Resources
digestEvents args = do
    ev <- SDL.pollEvent
    case ev of
        SDL.NoEvent -> return args
        VideoResize w h -> reshape (w,h) args >>= digestEvents
        KeyDown (Keysym SDLK_ESCAPE _ _) -> exitSuccess
        KeyDown (Keysym SDLK_RIGHT _ _) -> 
            digestEvents $ setDifEyeLocation (difEyeLocation args +++ (0,1,0)) args
        KeyDown (Keysym SDLK_LEFT _ _) -> 
            digestEvents $ setDifEyeLocation (difEyeLocation args +++ (0,-1,0)) args
        KeyUp (Keysym SDLK_LEFT _ _)-> 
            digestEvents $ setDifEyeLocation (difEyeLocation args +++ (0,1,0)) args
        KeyUp (Keysym SDLK_RIGHT _ _)-> 
            digestEvents $ setDifEyeLocation (difEyeLocation args +++ (0,-1,0)) args

        KeyDown (Keysym SDLK_UP _ _) -> 
            digestEvents $ setDifEyeLocation (difEyeLocation args +++ (0,0, 1)) args
        KeyDown (Keysym SDLK_DOWN _ _) -> 
            digestEvents $ setDifEyeLocation (difEyeLocation args +++ (0,0,-1)) args
        KeyUp (Keysym SDLK_UP _ _)-> 
            digestEvents $ setDifEyeLocation (difEyeLocation args +++ (0,0,-1)) args
        KeyUp (Keysym SDLK_DOWN _ _)-> 
            digestEvents $ setDifEyeLocation (difEyeLocation args +++ (0,0, 1)) args

        KeyDown (Keysym SDLK_w _ _) -> 
            digestEvents $ setDifEyeLocation (difEyeLocation args +++ (-0.1,0,0)) args
        KeyDown (Keysym SDLK_s _ _) -> 
            digestEvents $ setDifEyeLocation (difEyeLocation args +++ (0.1,0,0)) args
        KeyUp (Keysym SDLK_w _ _)-> 
            digestEvents $ setDifEyeLocation (difEyeLocation args +++ (0.1,0,0)) args
        KeyUp (Keysym SDLK_s _ _)-> 
            digestEvents $ setDifEyeLocation (difEyeLocation args +++ (-0.1,0,0)) args

        KeyDown (Keysym SDLK_n _ _) ->
            digestEvents $ (Prelude.flip setUseNoise args.not.useNoise) args

        KeyDown (Keysym SDLK_EQUALS _ _) ->
            digestEvents $ setDTime (dTime args + 1.0) args

        KeyDown (Keysym SDLK_MINUS _ _) ->
            digestEvents $ setDTime (dTime args - 1.0) args

        Quit -> exitSuccess
        _ -> digestEvents args
    where
        (+++) = zipWithT3 (+)

reshape :: (Int, Int) -> Resources -> IO Resources
reshape (w, h) args = do
    let size = Size (fromIntegral w) (fromIntegral h)
    let pMatrix' = perspectiveMatrix 50 (fromIntegral w / fromIntegral h) 0.1 100
    viewport $=(Position 0 0, size)
    _ <- SDL.setVideoMode w h 32 [SDL.OpenGL, SDL.Resizable, SDL.DoubleBuf]
    return $ setPMatrix pMatrix' args

bindSurfaceToTexture :: SDL.Surface -> TextureObject -> IO TextureData
bindSurfaceToTexture surf to = do
    textureBinding Texture2D $= Just to
    bbp <- liftM fromIntegral (pixelFormatGetBytesPerPixel $ surfaceGetPixelFormat surf)
    ptr <- surfaceGetPixels surf
    glTexImage2D gl_TEXTURE_2D 0 bbp (w surf) (h surf) 0 (if bbp == 3 then gl_RGB else gl_RGBA) gl_UNSIGNED_BYTE ptr
    return $ TextureData (w surf, h surf) to
    where 
          w :: (Integral a) => SDL.Surface -> a
          w = fromIntegral . surfaceGetWidth
          h :: (Integral a) => SDL.Surface -> a
          h = fromIntegral . surfaceGetHeight

textureFromSurface :: SDL.Surface -> IO TextureData
textureFromSurface surf = makeTexture >>= (bindSurfaceToTexture surf >=> return)

genRandomTexture :: TextureObject -> IO TextureData
genRandomTexture to =
    -- putStrLn ("takeShot")
    let nextColor gen =
         let (g1, gen') = next gen in
         let (g2, gen'') = next gen' in
         let (g3, gen''') = next gen'' in
         let (g4, gen'''') = next gen''' in
         ((g1,g2,g3,g4),gen'''') in do

    stgen <- newStdGen
    mPix <- newPixelsFromListRGBA (1024,1024) (randomTup $ randoms stgen)

    attachPixelsToTexture mPix to
    return $ TextureData (1024,1024) to
    where randomTup (a:b:c:d:xs) = (a,b,c,d):randomTup xs

main :: IO ()
main = do
        let _printError = get errors >>= mapM_ (putStrLn . ("GL: "++) . show)
        let size@(w,h) = (640, 480)
    
        SDL.init [SDL.InitEverything]

        _ <- SDL.setVideoMode w h 32 [SDL.OpenGL, SDL.Resizable, SDL.DoubleBuf]
        screen <- SDL.getVideoSurface
        resources <- makeResources
        reshape size resources >>= mainloop screen

    where mainloop screen resources =
            digestEvents resources >>= display screen >>= (mainloop screen . updateResources)
          (+++) = zipWithT3 (+)
          updateResources res =
            setEyeLocation (zipWithT3 (+) (eyeLocation res) (difEyeLocation res)) $
            setResTime ( resTime res + (dTime res) ) res

