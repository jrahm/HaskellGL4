{-# LANGUAGE MultiParamTypeClasses #-}
module Graphics.Glyph.Mat4 where

import Control.Monad

import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable

import Graphics.Rendering.OpenGL
import Graphics.Rendering.OpenGL.Raw.Core31

data Mat4 a = Matrix (a,a,a,a,
                      a,a,a,a,
                      a,a,a,a,
                      a,a,a,a) | IdentityMatrix

data Mat3 a = Matrix3 ( a,a,a,
                        a,a,a,
                        a,a,a ) | IdentityMatrix3

class StorableMatrix t a where
    fromList :: [t] -> a t
    toPtr :: a t -> (Ptr t -> IO b) -> IO b
    fromPtr :: Ptr t -> (a t -> IO b) -> IO b

instance (Storable t) => StorableMatrix t Mat4 where
    fromList (m1:m2:m3:m4:m5:m6:m7:m8:m9:m10:m11:m12:m13:m14:m15:m16:_) =
        Matrix (m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,m14,m15,m16)

    toPtr (Matrix (m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,m14,m15,m16)) fun =
        allocaArray 16 $ \ptr -> do
            pokeArray ptr [m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,m14,m15,m16]
            fun ptr

    fromPtr ptr f = peekArray 16 ptr >>= f . fromList

instance (Storable t) => StorableMatrix t Mat3 where
    fromList (m1:m2:m3:m4:m5:m6:m7:m8:m9:_) =
        Matrix3 (m1,m2,m3,m4,m5,m6,m7,m8,m9)

    toPtr (Matrix3 (m1,m2,m3,m4,m5,m6,m7,m8,m9)) fun =
        allocaArray 9 $ \ptr -> do
            pokeArray ptr [m1,m2,m3,m4,m5,m6,m7,m8,m9]
            fun ptr

    fromPtr ptr f = peekArray 9 ptr >>= f . fromList

instance Uniform (Mat4 GLfloat) where
    uniform (UniformLocation loc)  = makeStateVar getter setter
                    where setter mat = toPtr mat $ \ptr ->
                                    glUniformMatrix4fv loc 1 (fromIntegral gl_FALSE) ptr
                          getter :: IO (Mat4 GLfloat)
                          getter = do
                                pid <- liftM fromIntegral getCurrentProgram
                                ( allocaArray 16 $ \buf -> do
                                    glGetUniformfv pid loc buf
                                    fromPtr buf return )

instance Uniform (Mat3 GLfloat) where
    uniform (UniformLocation loc)  = makeStateVar getter setter
                    where setter mat = toPtr mat $ \ptr ->
                                    glUniformMatrix3fv loc 1 (fromIntegral gl_FALSE) ptr
                          getter :: IO (Mat3 GLfloat)
                          getter = do
                                pid <- liftM fromIntegral getCurrentProgram
                                ( allocaArray 9 $ \buf -> do
                                    glGetUniformfv pid loc buf
                                    fromPtr buf return )

getCurrentProgram :: IO GLint
getCurrentProgram = alloca $ glGetIntegerv gl_CURRENT_PROGRAM >> peek

instance (Show a) => Show (Mat4 a) where
    show IdentityMatrix =
        "[ 1 0 0 0\n" ++
        "  0 1 0 0\n" ++
        "  0 0 1 0\n" ++
        "  0 0 0 1 ]\n"
    show (Matrix (m00,m01,m02,m03,m10,m11,m12,m13,m20,m21,m22,m23,m30,m31,m32,m33)) =
        "["++! m00 ++ " " ++! m01 ++ " " ++! m02 ++ " " ++! m03 ++ "\n" ++
        " "++! m10 ++ " " ++! m11 ++ " " ++! m12 ++ " " ++! m13 ++ "\n" ++
        " "++! m20 ++ " " ++! m21 ++ " " ++! m22 ++ " " ++! m23 ++ "\n" ++
        " "++! m30 ++ " " ++! m31 ++ " " ++! m32 ++ " " ++! m33 ++ "]"
     where (++!) a = (a++) . show
        

    

translateMat4 :: (Num a) => Mat4 a -> (a,a,a,a) -> Mat4 a
translateMat4 IdentityMatrix x = translateMat4 (Matrix (1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1)) x
translateMat4  (Matrix (m00,m01,m02,m03,
                        m10,m11,m12,m13,
                        m20,m21,m22,m23,
                        m30,m31,m32,m33)) (v0,v1,v2,v3) =
                Matrix (m00,m01,m02,m03+v0,
                        m10,m11,m12,m13+v1,
                        m20,m21,m22,m23+v2,
                        m30,m31,m32,m33+v3)

applyMatrix :: (Num a) => Mat4 a -> (a,a,a,a) -> (a,a,a,a)
applyMatrix  (Matrix (m00,m01,m02,m03,
              m10,m11,m12,m13,
              m20,m21,m22,m23,
              m30,m31,m32,m33)) (v0,v1,v2,v3) =
                ( v0 * m00 + v1 * m01 + v2 * m02 + v3 * m03,
                  v0 * m10 + v1 * m11 + v2 * m12 + v3 * m13,
                  v0 * m20 + v1 * m21 + v2 * m22 + v3 * m23,
                  v0 * m30 + v1 * m31 + v2 * m32 + v3 * m33 )

applyMatrix IdentityMatrix v = v

scaleMatrix :: (Num a) => Mat4 a -> (a,a,a) -> Mat4 a
scaleMatrix IdentityMatrix (a,b,c) = Matrix ( a,0,0,0,
                               0,b,0,0,
                               0,0,c,0,
                               0,0,0,1)

scaleMatrix (Matrix (m00,m01,m02,m03,m10,m11,m12,m13,m20,m21,m22,m23,m30,m31,m32,m33)) (a,b,c)
                    = Matrix ( m00*a,m01,m02,m03,
                               m10,m11*b,m12,m13,
                               m20,m21,m22*c,m23,
                               m30,m31,m32,m33)

applyMatrixToList :: (Num a) => Mat4 a -> [a] -> [a]
applyMatrixToList IdentityMatrix t = t
applyMatrixToList mat (a:b:c:xs) =
    let (a',b',c',_) = applyMatrix mat (a,b,c,1) in
    (a':b':c':applyMatrixToList mat xs)

applyMatrixToList _ _ = []

mulMatrix4 :: (Num a) => Mat4 a -> Mat4 a -> Mat4 a
mulMatrix4 IdentityMatrix a = a
mulMatrix4 a IdentityMatrix = a
mulMatrix4
    (Matrix (a00,a01,a02,a03,
              a10,a11,a12,a13,
              a20,a21,a22,a23,
              a30,a31,a32,a33 ))
    (Matrix (b00,b01,b02,b03,
              b10,b11,b12,b13,
              b20,b21,b22,b23,
              b30,b31,b32,b33 )) =
                Matrix  (b00*a00+b10*a01+b20*a02+b30*a03,
                         b01*a00+b11*a01+b21*a02+b31*a03,
                         b02*a00+b12*a01+b22*a02+b32*a03,
                         b03*a00+b13*a01+b23*a02+b33*a03,

                         b00*a10+b10*a11+b20*a12+b30*a13,
                         b01*a10+b11*a11+b21*a12+b31*a13,
                         b02*a10+b12*a11+b22*a12+b32*a13,
                         b03*a10+b13*a11+b23*a12+b33*a13,

                         b00*a20+b10*a21+b20*a22+b30*a23,
                         b01*a20+b11*a21+b21*a22+b31*a23,
                         b02*a20+b12*a21+b22*a22+b32*a23,
                         b03*a20+b13*a21+b23*a22+b33*a23,

                         b00*a30+b10*a31+b20*a32+b30*a33,
                         b01*a30+b11*a31+b21*a32+b31*a33,
                         b02*a30+b12*a31+b22*a32+b32*a33,
                         b03*a30+b13*a31+b23*a32+b33*a33 )

(|*|) :: (Num a) => Mat4 a -> Mat4 a -> Mat4 a
(|*|) = mulMatrix4

transpose4 :: Mat4 a -> Mat4 a
transpose4 (Matrix
                (m00,m01,m02,m03,
                 m10,m11,m12,m13,
                 m20,m21,m22,m23,
                 m30,m31,m32,m33 ))  = (Matrix (m00, m10, m20, m30,
                                                m01, m11, m21, m31,
                                                m02, m12, m22, m32,
                                                m03, m13, m23, m33))
scale4 :: (Num a) => a -> Mat4 a -> Mat4 a
scale4 n (Matrix (m11,m12,m13,m14,m21,m22,m23,m24,m31,m32,m33,m34,m41,m42,m43,m44)) =
    Matrix (m11*n,m12*n,m13*n,m14*n,m21*n,m22*n,m23*n,m24*n,m31*n,m32*n,m33*n,m34*n,m41*n,m42*n,m43*n,m44*n)

det4 :: (Num a) => Mat4 a -> a
det4 (Matrix (m11,m12,m13,m14,m21,m22,m23,m24,m31,m32,m33,m34,m41,m42,m43,m44)) =
          m11*m22*m33*m44 + m11*m23*m34*m42 + m11*m24*m32*m43
        + m12*m21*m34*m43 + m12*m23*m31*m44 + m12*m24*m33*m41
        + m13*m21*m32*m44 + m13*m22*m34*m41 + m13*m24*m31*m42
        + m14*m21*m33*m42 + m14*m22*m31*m43 + m14*m23*m32*m41
        - m11*m22*m34*m43 - m11*m23*m32*m44 - m11*m24*m33*m42
        - m12*m21*m33*m44 - m12*m23*m34*m41 - m12*m24*m31*m43
        - m13*m21*m34*m42 - m13*m22*m31*m44 - m13*m24*m32*m41
        - m14*m21*m32*m43 - m14*m22*m33*m41 - m14*m23*m31*m42

inv4 :: (Floating a,Eq a) => Mat4 a -> Maybe (Mat4 a)
inv4 mat@(Matrix (m11,m12,m13,m14,m21,m22,m23,m24,m31,m32,m33,m34,m41,m42,m43,m44)) =
        let b11 = m22*m33*m44 + m23*m34*m42 + m24*m32*m43 - m22*m34*m43 - m23*m32*m44 - m24*m33*m42
            b12 = m12*m34*m43 + m13*m32*m44 + m14*m33*m42 - m12*m33*m44 - m13*m34*m42 - m14*m32*m43
            b13 = m12*m23*m44 + m13*m24*m42 + m14*m22*m43 - m12*m24*m43 - m13*m22*m44 - m14*m23*m42
            b14 = m12*m24*m33 + m13*m22*m34 + m14*m23*m32 - m12*m23*m34 - m13*m24*m32 - m14*m22*m33
            b21 = m21*m34*m43 + m23*m31*m44 + m24*m33*m41 - m21*m33*m44 - m23*m34*m41 - m24*m31*m43
            b22 = m11*m33*m44 + m13*m34*m41 + m14*m31*m43 - m11*m34*m43 - m13*m31*m44 - m14*m33*m41
            b23 = m11*m24*m43 + m13*m21*m44 + m14*m23*m41 - m11*m23*m44 - m13*m24*m41 - m14*m21*m43
            b24 = m11*m23*m34 + m13*m24*m31 + m14*m21*m33 - m11*m24*m33 - m13*m21*m34 - m14*m23*m31
            b31 = m21*m32*m44 + m22*m34*m41 + m24*m31*m42 - m21*m34*m42 - m22*m31*m44 - m24*m32*m41
            b32 = m11*m34*m42 + m12*m31*m44 + m14*m32*m41 - m11*m32*m44 - m12*m34*m41 - m14*m31*m42
            b33 = m11*m22*m44 + m12*m24*m41 + m14*m21*m42 - m11*m24*m42 - m12*m21*m44 - m14*m22*m41
            b34 = m11*m24*m32 + m12*m21*m34 + m14*m22*m31 - m11*m22*m34 - m12*m24*m31 - m14*m21*m32
            b41 = m21*m33*m42 + m22*m31*m43 + m23*m32*m41 - m21*m32*m43 - m22*m33*m41 - m23*m31*m42
            b42 = m11*m32*m43 + m12*m33*m41 + m13*m31*m42 - m11*m33*m42 - m12*m31*m43 - m13*m32*m41
            b43 = m11*m23*m42 + m12*m21*m43 + m13*m22*m41 - m11*m22*m43 - m12*m23*m41 - m13*m21*m42
            b44 = m11*m22*m33 + m12*m23*m31 + m13*m21*m32 - m11*m23*m32 - m12*m21*m33 - m13*m22*m31 in
            case det4 mat of
                0 -> Nothing
                det -> Just $ (1 / det) `scale4` Matrix (b11,b12,b13,b14,b21,b22,b23,b24,b31,b32,b33,b34,b41,b42,b43,b44)

trunc4 :: Mat4 a -> Mat3 a
trunc4 (Matrix
        (m11,m12,m13,_,
         m21,m22,m23,_,
         m31,m32,m33,_,
          _ , _ , _ ,_)) = Matrix3 (m11,m12,m13,m21,m22,m23,m31,m32,m33)

toNormalMatrix :: (Floating a,Eq a) => Mat4 a -> Maybe (Mat3 a)
toNormalMatrix mat = inv4 mat >>= return . trunc4 . transpose4
