import Lists1

data CounterElem a = Single a | Multiple Int a deriving Show

p11helper ::  (Int, a) -> CounterElem a
p11helper (1, x) = Single x
p11helper (x, y) = Multiple x y

p11 :: (Eq a) => [a] -> [CounterElem a]
p11 [] = []
p11 xs = map p11helper (p10v3 xs)

p12helper :: CounterElem a -> [a]
p12helper (Single x) = [x]
p12helper (Multiple x y) = replicate x y

p12 :: [CounterElem a] -> [a]
p12 [] = []
p12 (x:xs) = (p12helper x) ++ (p12 xs)

p14 :: [a] -> [a]
p14 [] = []
p14 (x:xs) = x:(x:(p14 xs))

p15 :: [a] -> Int -> [a]
p15 [] _ = []
p15 _ 0 = []
p15 (x:xs) n = (replicate n x)++(p15 xs n)

p16 :: [a] -> Int -> [a]
p16 [] _ = []
p16 list n = helper list n n
  where helper [] _ _ = []
        helper (x:xs) count 1 = helper xs count count
        helper (x:xs) count n = x : (helper xs count (n - 1))
        
p17 :: [a] -> Int -> ([a],[a])
p17 [] _ = ([],[])
p17 list 0 = ([],list)
p17 list n = helper list n n
  where helper [] _ _ = ([], [])
        helper list n 0 = ([], list)
        helper (x:xs) n m = let helperResult = helper xs n (m-1)
                            in (x : (fst (helperResult)) , (snd (helperResult)))
                            
p18 :: [a] -> Int -> Int -> [a]
p18 [] _ _ = []
p18 (x:xs) i j
   | i > (length (x:xs)) = []
   | i > j = []
   | ((i == 1) && (j /= 1)) = x : p18 xs 1 (j - 1) 
   | otherwise = p18 xs (i - 1) (j - 1)   

p19 :: [a] -> Int -> [a]
p19 [] _ = []
p19 list 0 = list
p19 list i 
    | i > 0 =  p19 ((last list) : (init list)) (i-1)
    | otherwise = p19 ((tail list) ++ [head list]) (i + 1) 	
