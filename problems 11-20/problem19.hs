
nBegin :: [a] -> Integer -> [a]
nBegin (x:t) k
    | k == 1 = [x]
    | otherwise = [x] ++ nBegin t (k-1)
nEnd :: [a] -> Integer -> [a]
nEnd (x:t) k
    | k == 1 = [x] ++ t
    | otherwise = nEnd t (k-1)
count :: [a] -> Integer
count [] = 0
count (x:t) = 1 + count t
rotate :: [a] -> Integer -> [a]
rotate l k
    | k > 0 = nEnd l d ++ nBegin l d
    | otherwise = nEnd l (count l + 1 - d) ++ nBegin l (count l - d) where
        d = m `mod` (count l)
        m = abs k

main = do
    print( rotate ['a','b','c','d','e','f','g','h'] (-2))