
compress [] = []
compress (x:t) 
    | t == [] = [x]
    | x == head(t) = compress(t)
    | otherwise = [x] ++ compress(t)

main = do
    print(compress "aaaabccaadeeee")