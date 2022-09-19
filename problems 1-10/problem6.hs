isPalindrome [] = True
isPalindrome (x:t) 
    | t == [] = (x == last(t)) && isPalindrome(init(t))
    | otherwise = True
main = do
    print(isPalindrome [1,2,4,8,16,8,4,2,1])