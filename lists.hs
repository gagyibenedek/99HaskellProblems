basic a = a * a
basic2 b = b + b

xs = [1,2,3,4,5,6,7,8,9]

p2 xs = xs !! (length xs - 2)

p3 xs  i = xs !! i

p4 xs = length xs

p5 xs = reverse xs

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
p9 []   = []
p9 [x]  = [[x]]
p9 (x:xs) = if x `elem` (head (p9 xs))
          then (x:(head (p9 xs))):(tail (p9 xs))
		  else [x]:(p9 xs)