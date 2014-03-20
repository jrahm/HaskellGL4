{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Graphics.Glyph.BufferBuilder where

import Control.Monad
import Graphics.Rendering.OpenGL
import Foreign.Storable
import Foreign.Ptr
import Data.Array.Storable
import Data.Setters
import Debug.Trace
import qualified Data.Foldable as Fold
import Data.Sequence as Seq

import Graphics.Glyph.Mat4
import Graphics.Glyph.Util

import System.IO.Unsafe

data BufferBuilder3D = Plot BufferBuilder3D (GLfloat,GLfloat,GLfloat) Int Int | End
bufferSize :: BufferBuilder3D -> Int
bufferSize End = 0
bufferSize (Plot _ _ l _) = l

nelem :: BufferBuilder3D -> Int
nelem End = 0
nelem (Plot _ _ _ l) = l

sizeofGLfloat :: Int
sizeofGLfloat = 4

{- A state monad that keeps track of operations
 - and will compile them into a buffer -}
data Builder b a = Builder {
    bList :: Seq (BuildDatum b),
    bReturn :: a
} | BuildError String

data BuildDatum b =
    VertexLink (b,b,b)  |
    NormalLink (b,b,b)  |
    ColorLink (b,b,b,b) |
    TextureLink (b,b) deriving Show

data CompiledBuild b = CompiledBuild {
    bStride :: Int,
    bEnabled :: (Bool,Bool,Bool),
    nElems  :: Int,
    array :: StorableArray Int b
}

bufferLength :: (Integral a) => CompiledBuild b -> a
bufferLength = fromIntegral . nElems

instance Show (CompiledBuild x) where
    show (CompiledBuild stride enabled n _) =
        "[CompiledBuild stride="++!stride++" enabled"++!enabled++" n="++!n++"]"

instance (Num t) => Monad (Builder t) where
    (Builder lst1 _) >> (Builder lst2 ret) = Builder (lst2 >< lst1) ret
    BuildError str >> _ = BuildError str
    _ >> BuildError str = BuildError str

    b1@(Builder _ ret1) >>= func = b1 >> func ret1
    BuildError str >>= _ = BuildError str

    return = Builder empty
    fail = BuildError

{- Add a vertex to the current builder -}
bVertex3 :: (a,a,a) -> Builder a ()
bVertex3 vert = Builder (Seq.singleton $ VertexLink vert) ()

bTexture2 :: (a,a) -> Builder a ()
bTexture2 tex = Builder (Seq.singleton $ TextureLink tex) ()

bNormal3 :: (a,a,a) -> Builder a ()
bNormal3 norm = Builder (Seq.singleton $ NormalLink norm) ()

bColor4 :: (a,a,a,a) -> Builder a ()
bColor4 col = Builder (Seq.singleton $ ColorLink col) ()

compilingBuilder :: (Storable b, Num b, Show b) => Builder b x -> IO (CompiledBuild b)
compilingBuilder (Builder lst _) = do
    -- Size of the elements
    let sizeof = sizeOf $ tmp (Seq.index lst 0)
                  where tmp (VertexLink (a,_,_)) = a
                        tmp _ = 0
    {- Simply figure out what types of elementse
     - exist in this buffer -}
    let en@(bn,bc,bt) = Fold.foldl' (\(bn,bc,bt) ele ->
                                case ele of
                                    NormalLink _ -> (True,bc,bt)
                                    ColorLink _ -> (bn,True,bt)
                                    TextureLink _ -> (bn,bc,True)
                                    VertexLink _ -> (bn,bc,bt)) (False,False,False) lst
    {- Calculate the stride; number of floats per element -}
    let stride = (3 + (?)bn * 3 + (?)bc * 4 + (?)bt * 2) * sizeof
                    where (?) True = 1
                          (?) False = 0
    --             Cur color normal texture buffer
    let (nverts,_,_,_,buffer) =
         Fold.foldl' (\(num,cn,cc,ct,ll) ele ->
                   -- trace ("foldl " ++! ele) $
                    case ele of
                        NormalLink nn -> (num,nn,cc,ct,ll)
                        ColorLink nc -> (num,cn,nc,ct,ll)
                        TextureLink nt -> (num,cn,cc,nt,ll)
                        VertexLink vert ->
                         (num+1,cn,cc,ct,
                            ll >< (tp3 True vert >< tp3 bn cn >< tp4 bc cc >< tp2 bt ct)
                            )) ( 0, (0,0,0), (0,0,0,0), (0,0), Seq.empty ) (Seq.reverse lst)

    let blst = (Fold.toList buffer)
    arr <- blst `seq` newListArray (0,Seq.length buffer) blst
    let compiledRet = CompiledBuild stride en nverts arr
    compiledRet `seq` putStrLn ("Compiled: " ++! compiledRet ) `seq` return compiledRet
  where
      tp2 True (a,b)  = Seq.fromList [a,b]
      tp2 False _  = empty
 
      tp3 True (a,b,c) = Seq.fromList [a,b,c]
      tp3 False _ = empty
 
      tp4 True (a,b,c,d) = Seq.fromList [a,b,c,d]
      tp4 False _ = empty

storableArrayToBuffer :: (Storable el) => BufferTarget -> StorableArray Int el -> IO BufferObject
storableArrayToBuffer target arr = do
    let sizeof = sizeOf $ unsafePerformIO (readArray arr 0)
    [buffer] <- genObjectNames 1
    bindBuffer target $= Just buffer
    len <- getBounds arr >>= (\(a,b) -> return $ (b - a) * sizeof )
    withStorableArray arr $ \ptr ->
        bufferData target $= (fromIntegral len, ptr, StaticDraw)
    return buffer

vertexArrayDescriptor :: CompiledBuild GLfloat -> VertexArrayDescriptor GLfloat
vertexArrayDescriptor (CompiledBuild stride _ _ _) = VertexArrayDescriptor 3 Float (fromIntegral stride) (wordPtrToPtr 0)

normalArrayDescriptor :: CompiledBuild GLfloat -> Maybe (VertexArrayDescriptor GLfloat)
normalArrayDescriptor (CompiledBuild stride (True,_,_) _ _) =
                Just $ VertexArrayDescriptor 3 Float
                        (fromIntegral stride) (wordPtrToPtr (3*4))
normalArrayDescriptor  _ = Nothing

colorArrayDescriptor :: CompiledBuild GLfloat -> Maybe (VertexArrayDescriptor GLfloat)
colorArrayDescriptor (CompiledBuild stride tup@(_,True,_) _ _) =
                Just $ VertexArrayDescriptor 4 Float
                        (fromIntegral stride) (wordPtrToPtr (offset tup))
                where offset (b1,_,_) = if b1 then (6*4) else (3*4)
                        
colorArrayDescriptor  _ = Nothing

textureArrayDescriptor :: CompiledBuild GLfloat -> Maybe (VertexArrayDescriptor GLfloat)
textureArrayDescriptor (CompiledBuild stride tup@(_,_,True) _ _) =
                Just $ VertexArrayDescriptor 2 Float
                        (fromIntegral stride) (wordPtrToPtr (offset tup))
                where offset (b1,b2,_) = (3 + (ifp b1 3) + (ifp b2 4)) * 4
                      ifp b x = if b then x else 0  
textureArrayDescriptor  _ = Nothing
createBufferObject :: BufferTarget -> CompiledBuild GLfloat -> IO BufferObject
createBufferObject target (CompiledBuild _ _ _ arr) = storableArrayToBuffer target arr
