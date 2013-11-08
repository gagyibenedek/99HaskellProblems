p21 :: a -> [a] -> Int -> [a]
p21 x [] _ = [x]
p21 y (x:xs) i
    | i < 2 = y : (x:xs)
	| i > length (x:xs) = (x:xs) ++ [y]
	| otherwise = x : (p21 y xs (i - 1))