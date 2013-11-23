p31 :: Int -> Bool
p31 x = p31helper x (div x 2)

p31helper :: Int -> Int -> Bool
p31helper _ 1 = True
p31helper x i
    | i < 1 = False
	|  mod x i == 0 = False
	| otherwise = p31helper x (i-1)
	
p32 :: Int -> Int -> Int
p32 1 _ = 1
p32 _ 1 = 1
p32 x y
    | x == y = x 
    | otherwise = p32 (min x y) (abs (x - y))