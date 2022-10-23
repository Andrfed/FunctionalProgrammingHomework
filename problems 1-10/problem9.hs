pack :: [String] -> [String]
pack [] = []
pack [x] = [x]
pack (x:t) 
    | x /= ht = x:pack t
    | otherwise = (x ++ f) : tt
        where
            ht = head t
            r = pack t
            f = head r
            tt = tail r

main = do
    print(pack(["a", "b", "b", "c", "c", "c"]))