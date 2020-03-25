--MAP
soma2 :: Num a => [a] -> [a]
soma2 [] = []
soma2 (h:t) = [h+2] ++ soma2 t

--FILTER
remove2 :: [Int] -> [Int]
remove2 [] = []
remove2 (h:t) |h==2 = remove2 t
              |otherwise = (h:remove2 t)

--FOLDR e FOLDL
multList :: [Int] -> Int
multList [] = 1
multList (h:t) = h * multList t

--ex 47
-- listac f p x = [f x | x ← xs, p x]
listcc f p x = map f (filter p x)

--ex 48 a)
(+++) :: [a] -> [a] -> [a]
(+++) l p = foldr (:) p l

--ex 48 b)
concat2 :: [[a]] -> [a]
concat2 l = foldr (++) [] l

-- ex 48 c)
reverse2 :: [a] -> [a]
reverse2 l = foldr (\v l = l ++ [v]) 

--ex 48 e)
elem2 :: Eq a => a -> [a] -> Bool
elem2 n l = any (\x -> x==v) l

--ex 49
dec2int :: [Int] → Int
dec2int l = foldl (\x y -> x*10+y) 0 l