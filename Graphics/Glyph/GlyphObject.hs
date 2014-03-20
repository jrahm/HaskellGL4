{-# LANGUAGE TemplateHaskell #-}

module Graphics.Glyph.GlyphObject (
    GlyphObject,
    getBufferObject,
    getCompiledData,
    getVertexAttribute,
    getNormalAttribute,
    getColorAttribute ,
    getTextureAttribute,
    getResources,
    getSetupRoutine,
    getTeardownRoutine,
    getPrimitiveMode,
    setBufferObject,
    setCompiledData,
    setVertexAttribute,
    setNormalAttribute,
    setColorAttribute ,
    setTextureAttribute,
    setResources,
    setSetupRoutine,
    setTeardownRoutine,
    setPrimitiveMode,
    prepare, teardown,
    Drawable, draw, newGlyphObject,
    newDefaultGlyphObject
) where

import Graphics.Glyph.BufferBuilder
import Graphics.Glyph.Util
import Graphics.Rendering.OpenGL
import Data.Setters

import Control.Monad
import Control.Applicative
import Data.Maybe

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
    teardownRoutine :: (Maybe (GlyphObject a -> IO ())), -- Tear down
    primitiveMode :: PrimitiveMode
}

$(declareSetters ''GlyphObject)
getBufferObject :: GlyphObject a -> BufferObject
getBufferObject = bufferObject

getCompiledData :: GlyphObject a -> (CompiledBuild GLfloat)
getCompiledData = compiledData

getVertexAttribute :: GlyphObject a -> AttribLocation
getVertexAttribute = vertexAttribute

getNormalAttribute :: GlyphObject a -> (Maybe AttribLocation)
getNormalAttribute = normalAttribute

getColorAttribute  :: GlyphObject a -> (Maybe AttribLocation)
getColorAttribute  = colorAttribute

getTextureAttribute :: GlyphObject a -> (Maybe AttribLocation)
getTextureAttribute = textureAttribute

getResources :: GlyphObject a -> a
getResources = resources

getSetupRoutine :: GlyphObject a -> (Maybe (GlyphObject a -> IO ()))
getSetupRoutine = setupRoutine

getTeardownRoutine :: GlyphObject a -> (Maybe (GlyphObject a -> IO ()))
getTeardownRoutine = teardownRoutine

getPrimitiveMode :: GlyphObject a -> PrimitiveMode
getPrimitiveMode = primitiveMode

newGlyphObject :: Builder GLfloat x ->
    AttribLocation ->
    Maybe AttribLocation ->
    Maybe AttribLocation ->
    Maybe AttribLocation ->
    a ->
    Maybe (GlyphObject a -> IO ()) ->
    Maybe (GlyphObject a -> IO ()) ->
    PrimitiveMode ->
    IO (GlyphObject a)

newGlyphObject builder vertAttr normAttr colorAttr textureAttr res setup tear mode = do
    compiled <- compilingBuilder builder
    buffer <- createBufferObject ArrayBuffer compiled
    return $ GlyphObject buffer compiled vertAttr normAttr colorAttr textureAttr res setup tear mode

prepare :: GlyphObject a -> (GlyphObject a -> IO()) -> GlyphObject a
prepare a b = setSetupRoutine (Just b) a

teardown :: GlyphObject a -> (GlyphObject a -> IO()) -> GlyphObject a
teardown a b = setTeardownRoutine (Just b) a

instance Drawable (GlyphObject a) where
    draw obj@(GlyphObject bo co vAttr nAttr cAttr tAttr _ setup tearDown p) = do
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

        drawArrays p 0 (bufferLength co)
            
        forM_ enabled $ \(attr, _) -> do
            vertexAttribArray attr $= Disabled
            
        {- Tear down whatever the object needs -}
        maybe (return ()) (apply obj) tearDown
        where liftMaybe (Just a, Just b) = Just (a,b)
              liftMaybe _ = Nothing
              apply obj' f = f obj'

instance (Show a) => Show (GlyphObject a) where
    show (GlyphObject _ co vAttr nAttr cAttr tAttr res _ _ p) =
        "[GlyphObject compiled=" ++! co ++ " vertAttr=" ++! vAttr ++
            " normalAttr="++!nAttr++" colorAttr="++!cAttr++" textureAttr="++!tAttr++" res="++!res++" PrimitiveMode="++!p++"]"


newDefaultGlyphObject :: Builder GLfloat x -> a -> IO (GlyphObject a)
newDefaultGlyphObject builder resources =
                        newGlyphObject builder
                                       (AttribLocation 0) -- vertex
                                       (Just $ AttribLocation 1) -- normal
                                       (Just $ AttribLocation 2) -- color
                                       (Just $ AttribLocation 3) -- texture
                                       resources
                                       Nothing -- setup
                                       Nothing -- teardown
                                       Triangles -- primitive
        
    
