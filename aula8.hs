impares :: [Int]
impares = 1 : [n + 2 | n <- impares]

--ex 57
factorial :: [Int]
factorial = 1 : [a * b | (a, b) <- zip factorial [1..]]

fibonacci :: [Int]
fibonacci = 0 : 1 : [x + y | (x,y) <- zip fibonacci (tail fibonacci)]
--fibonacci = 0 : 1 : zipWith (+) fibonacci (tail fibonacci)

--ex 58
merge :: [Int] -> [Int] -> [Int]
merge (h:t) (s:ts) | h < s = h : merge t (s:ts)
                   | h > s = s : merge (h:t) ts
                   | otherwise = h : merge t ts

pot n = 1 : map (*n) (pot n)

hamming :: [Int]
hamming = 1 : (merge l1 (merge l2 l3))
        where l1 = map (*2) hamming
              l2 = map (*3) hamming
              l3 = map (*5) hamming

--ex 59
somas :: [Int] -> [Int]
somas l = 0 : zipWith (+) l (somas l)
--somas (h:t) = h : map (+x) (somas t)

--ex 60
nextPascal :: [Int] -> [Int]
nextPascal l = [1] ++ zipWith (+) l (tail l) ++ [1]

pascal :: [[Int]]
pascal = [1] : map (nextPascal) pascal 

--ex 61
shift :: [a] -> [a]
shift [] =  []
shift (h:t) = t ++ [h]

rotate :: [a] -> [[a]] 
rotate l = take (length l) (iterate shift l)