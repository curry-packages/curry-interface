{-
  Module with hidden declarations. 
-}
module Hidden ( MyClass(myOp), MyData(MyData)
              , fun2, MyClass2(myOp2) ) 
  where

class MyClass a where
  myOp :: a -> Int

instance MyClass Int where
  myOp = id

class MyClass2 a where
  myOp2 :: a -> Int

  -- This function is hidden in the interface:
  myHidden :: a -> Int
  myHidden _ = 42

instance MyClass2 Int where
  myOp2 = id

  myHidden = const 43

data MyData = MyData Int | MyHiddenCons Int

fun1 :: (MyClass a, Num a) => a -> Int
fun1 = myOp . (+ 1)

fun2 :: MyData -> Int
fun2 (MyData x)       = x
fun2 (MyHiddenCons x) = x