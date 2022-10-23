dropEvery' :: String -> Integer -> Integer -> String
dropEvery' [] _ _ = []
dropEvery' (x:t) k n 
    | k == n = dropEvery' t 1 n
    | otherwise = [x] ++ dropEvery' t (k+1) n
dropEvery :: String -> Integer -> String
dropEvery lst n = dropEvery' lst 1 n

main = do
    print(dropEvery "abcdefghik" 3)