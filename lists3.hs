p21 :: a -> [a] -> Int -> [a]
p21 x [] _ = [x]
p21 y (x:xs) i
    | i < 2 = y : (x:xs)
	| i > length (x:xs) = (x:xs) ++ [y]
	| otherwise = x : (p21 y xs (i - 1))
	
p22 :: Int -> Int -> [Int]
p22 a b 
    | a == b = [a]
	| a > b = p22 b a
	| otherwise = a : p22 (a+1) b
	
p26 :: Int -> [a] -> [[a]]
p26 0 _ = []
p26 _ [] = []
p26 1 (x:xs) = [x] : p26 1 xs
p26 i (x:xs) = (map (x:) (p26 (i-1) xs)) ++ (p26 i xs)

p28 :: Ord a => [[a]] -> [[a]]
p28 [] = []
p28 [x] = [x]
p28 (x:xs) = p28 [y| y <- xs, (length y) < (length x)] ++ [x] ++ p28 [z| z <- xs, (length z) >= (length x)]
