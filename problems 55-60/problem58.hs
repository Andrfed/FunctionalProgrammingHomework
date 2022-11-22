data Tree = Branch Char Tree Tree | Empty deriving Show

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


cbalTree 1 = [Branch 'x' Empty Empty]
cbalTree 2 = [Branch 'x' Empty (Branch 'x' Empty Empty), Branch 'x' (Branch 'x' Empty Empty) Empty]
cbalTree n = [Branch 'x' l r | k <- [n1, n2], l <- cbalTree k, r <- cbalTree (n2 - k + n1)] where
    n1 = div nn 2
    n2 = div nn 2 + mod nn 2
    nn = n-1

contains :: [Tree] -> Tree -> Bool
contains [] _ = False
contains (x:t) tree = (isEquals x tree) || (contains t tree)

getSym :: [Tree] -> [Tree]
getSym [] = []
getSym (x:t)
    | (symmetric x == True) && (contains t x == False) = (x:(getSym t))
    | otherwise = getSym t

symCbalTrees :: Integer -> [Tree]
symCbalTrees n = getSym (cbalTree n)

main = print (show (symCbalTrees 5))