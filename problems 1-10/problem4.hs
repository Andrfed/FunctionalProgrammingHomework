myLength :: [a] -> Integer
myLength [] = 0
myLength (x:t) = 1 + myLength(t)
main = do
    print(myLength [1,2,3,4])