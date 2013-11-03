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