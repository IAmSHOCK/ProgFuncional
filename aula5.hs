--ex 40
anyZero :: (Integer -> Integer) -> Integer -> Bool
anyZero f 0 = if (f 0) == 0 then True else False
anyZero f n = x || anyZero f (n-1)
			where x = if (f n) == 0 then True else False

--ex 41
sumFun :: (Integer -> Integer) -> Integer -> Integer
sumFun f 0 = f 0
sumFun f n = (f n) + sumFun f (n-1)

--ex 42 a)
insert2 :: Ord a => a -> [a] -> [a]
insert2 n l = [p | p <- l, p < n] ++ [n] ++ [q | q <- l, q > n]

--ex 42 b)
isort :: Ord a => [a] -> [a]
isort [] = []
isort (x:xs) = insert2 x (isort xs) 

--ex 43 a)
minimum2 :: Ord a => [a] -> a
minimum2 [x] = x
minimum2 (x:y:xs) = if x < y then minimum2 (x:xs) else minimum2(y:xs)

--ex 43 b)
delete2 :: Eq a => a -> [a] -> [a]
delete2 n l = [p | p <- l, p /= n] -- apaga todas as ocorrencias


--ex 44 b)
--msort :: Ord a => [a] -> [a]
--msort [] = []


--metades :: [a] -> ([a], [a])
