split'' :: String -> Integer -> String
split'' (x:t) n
    | n == 1 = t
    | otherwise = split'' t (n-1)
split' :: String -> Integer -> String
split' (x:t) n 
    | n == 1 = [x]
    | otherwise = [x] ++ split' t (n-1)
split :: String -> Integer -> (String, String)
split lst n = (split' lst n, split'' lst n)

main = do
    print(split "abcdefghik" 3)