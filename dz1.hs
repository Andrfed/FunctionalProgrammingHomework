data Solution = NoSolution | Value {x1::Float, x2::Float}

myFunc :: Float -> Float -> Float -> Solution
myFunc x y z 
    | d >= 0 = Value {x1 = a, x2 = b}
    | otherwise = NoSolution where
            d = y^2 - 4 * x * z
            a = (-y + sqrt d) / (2 * x)
            b = (-y - sqrt d) / (2 * x)
        
showSolution :: Solution -> String
showSolution sol = 
    case sol of (NoSolution) -> "no solution"
                (Value x1 x2) -> "solution " ++ "(" ++ show(x1) ++ "," ++ show(x2) ++ ")"

-- Ввод каждого коэффициента с новой строки
main = do
    a <- getLine
    b <- getLine
    c <- getLine
    print $ showSolution $ myFunc (read a :: Float) (read b :: Float) (read c :: Float)