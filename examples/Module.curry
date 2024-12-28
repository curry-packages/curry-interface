module Module where

class MyClass a where
  myOp :: a -> Int

instance MyClass Int where
  myOp = id

fun :: (MyClass a, Num a) => a -> Int
fun = myOp . (+ 1)

data MyData = MyData Int | MyHiddenCons Int

fun2 :: MyData -> Int
fun2 (MyData x)       = x
fun2 (MyHiddenCons x) = x