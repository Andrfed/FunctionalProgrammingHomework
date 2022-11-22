data Tree = Branch Integer Tree Tree | Empty deriving Show

isEquals :: Tree -> Tree -> Bool
isEquals Empty Empty = True
isEquals Empty _ = False
isEquals _ Empty = False
isEquals (Branch x (sub11) (sub12)) (Branch y (sub21) (sub22)) = (isEquals sub11 sub21) && (isEquals sub12 sub22)

mirror :: Tree -> Tree
mirror Empty = Empty
mirror (Branch x (sub1) (sub2)) = Branch x (mirror sub2) (mirror sub1)

symmetric :: Tree -> Bool
symmetric tree = isEquals tree (mirror tree)

add :: Tree -> Integer -> Tree
add Empty x = Branch x Empty Empty
add (Branch cur (sub1) (sub2)) x
    | x < cur = Branch cur (add sub1 x) (sub2)
    | otherwise = Branch cur (sub1) (add sub2 x)

addAll :: Tree -> [Integer] -> Tree
addAll tree [] = tree
addAll tree (x:t) = addAll (add tree x) t

construct :: [Integer] -> Tree
construct (x:t) = addAll (Branch x Empty Empty) t

main = print (show (construct [3, 2, 5, 7, 1]) ++ show (symmetric . construct $ [3, 2, 5, 7, 1]))