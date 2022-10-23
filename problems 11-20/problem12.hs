charToString :: Char -> String
charToString c = [c]

data Duplicate = Multiple Integer Char | Single Char deriving Show

repeatChar :: Char -> Integer -> String
repeatChar c 1 = charToString(c)
repeatChar c n = charToString(c) ++ repeatChar c (n-1)

decodeModified :: [Duplicate] -> String
decodeModified [] = []
decodeModified (x:t) = 
    case x of 
        Single a -> repeatChar a 1 ++ decodeModified t
        Multiple n a -> repeatChar a n ++ decodeModified t

main = do
    print(decodeModified 
       [Multiple 4 'a',Single 'b',Multiple 2 'c',
        Multiple 2 'a',Single 'd',Multiple 4 'e'])