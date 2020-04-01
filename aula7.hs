--ex 50
zipWith1 :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith1 f [] _  = [] 
zipWith1 f _ []  = [] 
zipWith1 f (x:xs) (y:ys) = (f x y) : (zipWith1 f xs ys)

--ex 51
insertt :: Ord a => a -> [a] -> [a]
insertt v [] = [v]
insertt v (h:t) | (v<h) = v:h:t
                | otherwise = h:(insertt v t)

isort :: Ord a => [a] -> [a]
isort l = foldr (\v l -> insertt v l) [] l

--ex 52 a)
shift :: [a] -> [a]
shift [] = []
shift (h:t) = t ++ [h]

--ex 52 b)
rotate :: [a] -> [[a]]
rotate l = foldr (\_ l -> l ++ [ shift (last l) ]) [l] (tail l)
--rotate l = reverse (foldr (\v l -> [ shift (head l) ] ++ l) [l] (tail l))

--ex 53 a)
maxl :: Ord a => [a] -> a
maxl l = foldl1 max l

--minr :: Ord a => [a] -> a
minrl l = foldr1 min l

--53 b)
foldr2 f l = foldr f (last l) (init l)

foldl2 f l = foldl f (head l) (tail l)

--ex 54 a)
add i 0 = i
add i j = succ (add i (pred j))

mult i 1 = i 
mult i j = add i (mult i (pred j))
 
exp2 i 0 = 1
exp2 i j = mult i (exp2 i (pred j))

--ex 54 b)
foldi :: (a -> a) -> a -> Integer -> a
foldi f q 0 = q
foldi f q i = f (foldi f q (pred i))

addfoldi i j  = foldi succ i j 

multfoldi i j = foldi (+i) 0 j

expfoldi i j  = foldi (*i) 1 j

--ex 54 c)