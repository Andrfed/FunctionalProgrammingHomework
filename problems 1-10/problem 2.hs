myButLast :: [a] -> a
myButLast lst = last(init(lst))
main = putStrLn(show(myButLast [1, 2, 3, 4]))