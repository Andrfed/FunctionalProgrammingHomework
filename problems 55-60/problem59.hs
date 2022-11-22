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

cbalTree :: Char -> Integer -> [Tree]
cbalTree ch 1 = [Branch ch Empty Empty]
cbalTree ch 2 = [Branch ch Empty (Branch ch Empty Empty), Branch ch (Branch ch Empty Empty) Empty]
cbalTree ch n = [Branch ch l r | k <- [n1, n2], l <- cbalTree ch k, r <- cbalTree ch (n2 - k + n1)] where
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
symCbalTrees n = getSym (cbalTree 'x' n)

getHeight :: Tree -> Integer
getHeight Empty = 0
getHeight (Branch ch sub1 sub2) = 1 + max (getHeight sub1) (getHeight sub2)

getWithHeight :: Integer -> [Tree] -> [Tree]
getWithHeight _ [] = []
getWithHeight n (x:t) 
    | getHeight x == n = x:(getWithHeight n t)
    | otherwise = getWithHeight n t

cbalAllTree :: Char -> Integer -> [Tree]
cbalAllTree _ 0 = []
cbalAllTree ch n = (cbalTree ch n) ++ (cbalAllTree ch (n-1))

hbalTree :: Char -> Integer -> [Tree]
hbalTree ch n = getWithHeight n (cbalAllTree ch (2^n))

main = print (show (take 4 $ hbalTree 'x' 3))