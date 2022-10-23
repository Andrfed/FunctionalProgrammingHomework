repli :: String -> Integer -> String
repli "" _ = ""
repli s 1 = s
repli s n = hs ++ (repli hs (n-1)) ++ (repli ts n)
    where
        hs = [head s]
        ts = tail s

main = do
    print(repli "abc" 3)