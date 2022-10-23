combinations' :: Integer -> String -> String -> [String]
combinations' 0 _ cmb = [cmb]
combinations' _ [] _ = []
combinations' k (x:t) cmb = combinations' (k-1) t (cmb ++ [x]) ++ combinations' k t cmb

combinations :: Integer -> String -> [String]
combinations k lst = combinations' k lst ""

main = do
    print(combinations 3 "abcdef")
