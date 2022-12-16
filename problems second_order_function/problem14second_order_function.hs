dupli' :: a -> [a]
dupli' x = [x,x]

toAll :: (a -> b) -> [a] -> [b]
toAll f [] = []
toAll f (x:t) = (f x):(toAll f t)

simplify :: [[a]] -> [a]
simplify [] = []
simplify (x:t) = x ++ (simplify t)

dupli :: [a] -> [a]
dupli lst = simplify (toAll dupli' lst)

main = do
    print (show (dupli [1, 2, 3]))