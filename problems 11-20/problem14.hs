dupli :: [a] -> [a]
dupli [] = []
dupli (x:t) = [x, x] ++ dupli t

main = do
    print(dupli [1, 2, 3])