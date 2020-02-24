
-- Ex 2
triangulo :: Float -> Float -> Float -> Bool
triangulo a b c | a < b + c = True
                | b < a + c = True
                | c < a + b = True
                | otherwise = False
      

-- Ex 3
area :: Float -> Float -> Float -> Float
area a b c = sqrt (s*(s-a)*(s-b)*(s-c))
             where s = (a+b+c)/2

-- Ex 4
metades :: [Int] -> ([Int], [Int])
metades l = (l1, l2)
              where l1 = take ((length l) `div` 2) l
                    l2 = drop ((length l) `div` 2) l


-- Ex 5
last :: [a] -> [a]
last l = drop ((length l) -1) l


-- Ex 6
--binom :: Int -> Int -> Float
binom n k = product [1..n] / ((product [1..k]) * (product[1..(n-k)]))
binom2 n k = product [1..n] / ((product [1..k]) * (product[1..(n-k)])) 


-- Ex 7

--max3::

max3 x y z =
    max t y
    where t = max x z

min3 x y z =
    min t y
    where t = min x z


-- Ex 8 
-- maxOccurs :: Integer -> Integer -> (Integer, Integer)
-- maxOccurs x y =
    

orderTriple :: (Integer, Integer, Integer) -> (Integer, Integer, Integer)
orderTriple (x, y, z) = 
    let c = max3 x y z 
        a = min3 x y z
        b = x+y+z-a-c
        in (a, b, c)

classifica:: Int -> String
classifica x = |x<=9 = reprovado
               |x<= 10 && x>=12 = suficiente
               |x<=13 && 