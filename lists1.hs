basic a = a * a
basic2 b = b + b

xs = [1,2,3,4,5,6,7,8,9]

p2 xs = xs !! (length xs - 2)

p3 xs  i = xs !! i

p4 xs = length xs

p4manual :: [a] -> Int
p4manual [] = 0;
p4manual (x:xs) = 1 + p4manual xs

p5 xs = reverse xs

p5manual :: [a] -> [a]
p5manual [] = []
p5manual (x:xs) = p5manual xs ++ [x]

p6 :: (Eq xs) => [xs] -> Bool
p6 xs = (reverse xs) == xs

data NestedList a = Elem a | List [NestedList a]

p7 :: NestedList a -> [a]
p7 (Elem a)      = [a]
p7 (List (x:xs))   = p7 x ++ p7 (List xs)
p7 (List [])     = []


p8 :: (Eq a) => [a] -> [a]
p8 [] = []
p8 [x] = [x]
p8 (x:xs) = if x == (head xs) then p8 xs
            else [x] ++ p8 xs;

p9 :: (Eq a) => [a] -> [[a]]
p9 [] = []
p9 [x] = [[x]]
p9 (x:xs) = if x `elem` (head (p9 xs))
            then (x:(head (p9 xs))):(tail (p9 xs))
            else [x]:(p9 xs)
            
p9v2 :: (Eq a) => [a] -> [[a]]
p9v2 [] = []
p9v2 [x] = [[x]]
p9v2 (x:xs) = if x `elem` (p9head)
            then (x:p9head):(tail p9computed)
            else [x]:(p9computed)   
            where p9computed = p9 xs
                  p9head = head p9computed

p10 :: Eq a => [a] -> [(Int, a)]
p10 [] = []
p10 [x] = [(1, x)]
p10 (x:xs) = if x == (snd (head (p10 xs)))
             then ((fst(head (p10 xs))+1), x):(tail (p10 xs))
			 else (1, x):(p10 xs)
			
count :: Eq a => [a] -> (Int, a)
count xs = (length xs, head xs)
			
p10v2 :: Eq a => [a] -> [(Int, a)]
p10v2 [] = []
p10v2 [x] = [(1, x)]		
p10v2 xs = map count (p9 xs)

p10v3 :: Eq a => [a] -> [(Int, a)]
p10v3 [] = []
p10v3 [x] = [(1, x)]
p10v3 (x:xs) = if x == (snd (p10head))
             then ((fst(p10head)+1), x):(tail (p10computed))
			 else (1, x):(p10computed)
			 where p10computed = p10v3 xs
			       p10head = head p10computed