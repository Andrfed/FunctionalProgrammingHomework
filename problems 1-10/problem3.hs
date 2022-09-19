elementAt :: [a] -> Integer -> a
elementAt (x:t) 1 = x
elementAt (x:t) k = elementAt t (k-1)
main = do
    print(elementAt [1,2,3,4] 2)