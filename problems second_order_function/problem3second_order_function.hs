data T = Element Integer | Num Integer deriving Show

myFoldl _ a [] = a
myFoldl f a (x:t) = myFoldl f a1 t
    where
        a1 = f a x

ff2 :: T -> Integer -> T
ff2 (Num 1) x = Element x
ff2 (Num n) _ = Num (n-1)
ff2 (Element x) _ = Element x

elementAt' :: T -> Integer
elementAt' (Element x) = x
elementAt' (Num n) = n

elementAt :: [Integer] -> Integer -> Integer
elementAt lst n = elementAt' (myFoldl ff2 (Num n) lst)

main = do
    print (show (elementAt [1,2,3] 3))