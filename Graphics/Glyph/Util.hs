module Graphics.Glyph.Util where

import Data.Angle
import Graphics.Rendering.OpenGL

uncurry7 :: (a -> b -> c -> d -> e -> f -> g -> h) -> (a,b,c,d,e,f,g) -> h
uncurry7 func (a,b,c,d,e,f,g) = func a b c d e f g

uncurry6 :: (a -> b -> c -> d -> e -> f -> g) -> (a,b,c,d,e,f) -> g
uncurry6 func (a,b,c,d,e,f) = func a b c d e f

uncurry5 :: (a -> b -> c -> d -> e -> f) -> (a,b,c,d,e) -> f
uncurry5 func (a,b,c,d,e) = func a b c d e

uncurry4 :: (a -> b -> c -> d -> e) -> (a,b,c,d) -> e
uncurry4 func (a,b,c,d) = func a b c d

uncurry3 :: (a -> b -> c -> e) -> (a,b,c) -> e
uncurry3 func (a,b,c) = func a b c

gsin :: (Floating a) => a -> a
gsin = sine . Degrees

gcos :: (Floating a) => a -> a
gcos = cosine . Degrees

toEuclidian :: (Floating a) => (a, a, a) -> (a, a, a)
toEuclidian (r, th, ph) = (
        -r * gsin th * gcos ph,
         r * gsin ph,
         r * gcos th * gcos ph
    )

mapT2 :: (a -> b) -> (a,a) -> (b,b) 
mapT2 f (a, b) = (f a, f b)

mapT3 :: (a -> b) -> (a,a,a) -> (b,b,b) 
mapT3 f (a, b, c) = (f a, f b, f c)

mapT4 :: (a -> b) -> (a,a,a,a) -> (b,b,b,b) 
mapT4 f (a, b, c, d) = (f a, f b, f c, f d)

mapT5 :: (a -> b) -> (a,a,a,a,a) -> (b,b,b,b,b) 
mapT5 f (a, b, c, d, e) = (f a, f b, f c, f d, f e)

mapT6 :: (a -> b) -> (a,a,a,a,a,a) -> (b,b,b,b,b,b) 
mapT6 f (a, b, c, d, e, _f) = (f a, f b, f c, f d, f e, f _f)

mapT7 :: (a -> b) -> (a,a,a,a,a,a,a) -> (b,b,b,b,b,b,b) 
mapT7 f (a, b, c, d, e, _f, g) = (f a, f b, f c, f d, f e, f _f, f g)

foldT2 :: (a -> b -> a) -> a -> (b,b) -> a
foldT2 f ini (x,y) = ini `f` x `f` y

foldT3 :: (a -> b -> a) -> a -> (b,b,b) -> a
foldT3 f ini (x,y,z) = ini `f` x `f` y `f` z

foldT4 :: (a -> b -> a) -> a -> (b,b,b,b) -> a
foldT4 f ini (x,y,z,w) = ini `f` x `f` y `f` z `f` w

foldT5 :: (a -> b -> a) -> a -> (b,b,b,b,b) -> a
foldT5 f ini (x,y,z,w,v) = ini `f` x `f` y `f` z `f` w `f` v

tup2Len :: (Real a,Floating b) => (a,a) -> b
tup2Len = sqrt . foldT2 (+) 0 . mapT2 ((**2).toFloating)

tup3Len :: (Real a,Floating b) => (a,a,a) -> b
tup3Len = sqrt . foldT3 (+) 0 . mapT3 ((**2).toFloating)

tup4Len :: (Real a,Floating b) => (a,a,a,a) -> b
tup4Len = sqrt . foldT4 (+) 0 . mapT4 ((**2).toFloating)

tup5Len :: (Real a,Floating b) => (a,a,a,a,a) -> b
tup5Len = sqrt . foldT5 (+) 0 . mapT5 ((**2).toFloating)

expand3 :: a -> (a,a,a)
expand3 t = (t,t,t)

expand4 :: a -> (a,a,a,a)
expand4 t = (t,t,t,t)

expand5 :: a -> (a,a,a,a,a)
expand5 t = (t,t,t,t,t)

expand6 :: a -> (a,a,a,a,a)
expand6 t = (t,t,t,t,t)

zipWithT2 :: (a -> b -> c) -> (a,a) -> (b,b) -> (c,c)
zipWithT2 fu (a, b) (d, e) = (fu a d, fu b e)

zipWithT3 :: (a -> b -> c) -> (a,a,a) -> (b,b,b) -> (c,c,c)
zipWithT3 fu (a, b, c) (d, e, f) = (fu a d, fu b e, fu c f)

zipWithT4 :: (a -> b -> c) -> (a,a,a,a) -> (b,b,b,b) -> (c,c,c,c)
zipWithT4 fu (a, b, c, d) (e, f, g, h) = (fu a e, fu b f, fu c g, fu d h)

toFloating :: (Real a, Floating b) => a -> b
toFloating = fromRational . toRational

(!!%) :: [a] -> Int -> a
(!!%) lst idx = lst !! (idx `mod` length lst)

(++!) :: (Show a) => String -> a -> String
(++!) str = (str++) . show

clamp :: (Ord a) => a -> (a, a) -> a 
clamp var (low, high) = min (max var low) high

floatVertex :: (GLfloat,GLfloat,GLfloat) -> Vertex3 GLdouble
floatVertex tup = uncurry3 Vertex3 (mapT3 toFloating tup)

floatVector :: (GLfloat,GLfloat,GLfloat) -> Vector3 GLdouble
floatVector tup = uncurry3 Vector3 (mapT3 toFloating tup)

-- Maps a function across a list, except this function
-- can also be given a state variable like how foldl
-- works
mapWith :: (s -> a -> (b,s)) -> s -> [a] -> ([b], s)
mapWith func state (x:xs) = 
    let (x',s') = func state x in
    let (l,s) = mapWith func s' xs in (x':l, s)

mapWith _ s [] = ([],s)
(>&>) :: (Monad m) => (a -> m b) -> (a -> m c) -> a -> m c
(>&>) f1 f2 a = f1 a >> f2 a
