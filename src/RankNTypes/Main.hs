{-# LANGUAGE RankNTypes #-}
module Main where

f0 :: Int -> Int
f0 = id

f1 :: a -> a
f1 = id

f2 :: (forall a. a -> a) -> (Int, Bool) -> (Int, Bool)
f2 f (i, b) = (f i, f b)

f3 :: ((forall a. a -> a) -> Int) -> Bool -> Bool
f3 _ = id

f3_ex :: Bool
f3_ex = f3 f True
  where
    f :: (forall a. a -> a) -> Int
    f g = 0

f3a :: ((forall a. a -> a) -> Int) -> Bool -> Bool
f3a f b = (f id == 3) && b

f3a_ex :: Bool
f3a_ex = f3a f True
  where
    f :: (forall a. a -> a) -> Int
    f g = g 3


f3b :: ((forall a. a -> a -> a) -> Int) -> Bool -> Bool
f3b f b = (f const == 3) && b

f3b_ex :: Bool
f3b_ex = f3b f True where
   f :: (forall a. a -> a -> a) -> Int
   f g = g 3 5


f3c :: ((forall a. a -> a -> a) -> Int) -> Bool -> Bool
f3c f b = f (if b then const else const id) == 42

f3c_ex :: Bool
f3c_ex = f3c f True where
   f :: (forall a. a -> a -> a) -> Int
   f g = g 3 5 + 39

main = do
  print $ f0 5 -- 5
  print $ f1 "john" -- john
  print $ f2 id (3, True) -- (3, True)
  print f3_ex -- True
  print f3a_ex -- True
  print f3b_ex -- True
  print f3c_ex -- True

