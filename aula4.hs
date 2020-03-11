--ex1
expoente ::Num a => a -> a
expoente 0 = 1
expoente n = 2 * expoente (n-1)

--ex 32 a)
and1 :: [Bool] -> Bool
and1 [x] = x
and1 (x:xs)
    |x == False = False 
    |otherwise = and1 xs

--ex 32 b)
or1 :: [Bool] ->Bool
or1 [] = False
or1 (x:xs) 
    |x == True = True
    |otherwise = or1 xs

--ex 32 c)
concat1 :: [[a]] -> [a]
concat1 [] = []
concat1 (x:xs) = x ++ (concat1 xs)

--ex 32 d)
replicate1 :: Int -> a -> [a]
replicate1 0 v = []
replicate1 n v = [v] ++ replicate1 (n-1) v

--ex32 e)
(!!!) :: [a] -> Int -> a
(!!!) (x:xs) 0 = x
(!!!) (x:xs) n = (!!!) xs (n-1)

--ex32 f)
elem1 :: Eq a => a -> [a] -> Bool
elem1 n [] = False
elem1 n (x:xs)
    |x == n = True
    |otherwise = elem1 n xs

--ex33
concat2 :: [[a]] -> [a]
concat2 xss = [x | xs<-xss, x<-xs]

replicate2 :: Int -> a -> [a]
replicate2 n a = [a | _<- [1..n]]