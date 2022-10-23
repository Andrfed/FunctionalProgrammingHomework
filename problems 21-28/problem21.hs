
insertAt :: Char -> String -> Integer -> String
insertAt ch [] _ = [ch]
insertAt ch (x:t) pos
    | pos == 1 = [ch] ++ (x:t)
    | otherwise = [x] ++ insertAt ch t (pos-1)

main = do
    print(insertAt 'X' "abcd" 2)