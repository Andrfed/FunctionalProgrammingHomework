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

charToString :: Char -> String
charToString c = [c]

toList :: String -> [String]
toList [] = []
toList (x:t) = [charToString(x)] ++ toList(t)

count :: String -> Integer
count [] = 0
count (x:t) = 1 + count(t)

smap :: [String] -> [(Integer, Char)]
smap [] = []
smap (x:t) = [(count(x), head(x))] ++ smap(t)

encode :: String -> [(Integer, Char)]
encode s = smap $ pack $ toList s

data Duplicate = Multiple Integer Char | Single Char deriving Show

toDuplicate :: (Integer, Char) -> Duplicate
toDuplicate (1, c) = Single c
toDuplicate (n, c) = Multiple n c

encode_modified :: String -> [Duplicate]
encode_modified s = map toDuplicate $ encode s

main = do
    print(encode_modified "aaaabccaadeeee")