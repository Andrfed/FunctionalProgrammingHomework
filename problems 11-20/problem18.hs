
slice' :: [a] -> Integer -> [a]
slice' (x:t) i 
    | i == 1 = [x] ++ t
    | otherwise = slice' t (i-1)
slice'' :: [a] -> Integer -> [a]
slice'' (x:t) i
    | i == 1 = [x]
    | otherwise = [x] ++ slice'' t (i-1)
slice :: [a] -> Integer -> Integer -> [a]
slice l i k = slice'' (slice' l i) (k-i+1)

main = do
    print(slice ['a','b','c','d','e','f','g','h','i','k'] 3 7)