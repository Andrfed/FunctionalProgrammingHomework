data Tree = Node Char Tree Tree | Nil deriving Show
cbalTree 1 = [Node 'x' Nil Nil]
cbalTree 2 = [Node 'x' Nil (Node 'x' Nil Nil), Node 'x' (Node 'x' Nil Nil) Nil]
cbalTree n = [Node 'x' l r | k <- [n1, n2], l <- cbalTree k, r <- cbalTree (n2 - k + n1)] where
    n1 = div nn 2
    n2 = div nn 2 + mod nn 2
    nn = n-1

main = print $ show (cbalTree 4)