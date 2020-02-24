
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

classifica :: Int -> String
classifica s 
           | s <= 9             = "reprovado"
           | s >= 10 && s <= 12 = "suficiente"
           | s >= 13 && s <= 15 = "bom"
           | s >= 16 && s <= 18 = "muito bom"
           | s == 19 || s == 20 = "muito bom com distincao"
           | otherwise          = "nota invalida"

xor :: Bool -> Bool -> Bool 
xor x y 
     | x /= y = True
     | x == y = False

safetail :: [a] -> [a]
safetail [] = []
safetail x = tail x

curta :: [a] -> Bool
curta x
      | length x == 1 ||  length x == 2 || length x == 3 = True
      | otherwise = False



textual:: Int -> String
textual 0 = "zero"
textual x 
        | x == 1           = "um"
        | x == 2           = "dois"
        | x == 3           = "tres"
        | x == 4           = "quatro"
        | x == 5           = "cinco"
        | x == 6           = "seis"
        | x == 7           = "sete"
        | x == 8           = "oito"
        | x == 9           = "nove"
        | x == 10          = "dez"
        | x == 11          = "onze"
        | x == 12          = "doze"
        | x == 13          = "treze"
        | x == 14          = "quatorze"
        | x == 15          = "quinze"
        | x == 18          = "dezoito"
        | x == 16 || x ==17 || x == 19  = textual 10 ++ "a" ++textual(x-10)
        | x == 20          = "vinte"
        | x >= 21 && x <= 29 = "vinte e " ++ textual(x-20)
        | x == 30 = "trinta"
        | x >= 31 && x <= 39 = "trinta e " ++ textual (x-30)
        | x == 40 = "quarenta"
        | x >= 41 && x <= 49 = "quarenta e " ++ textual (x-40)
        | x == 50 = "cinquenta"
        | x >= 51 && x <= 59 = "cinquenta e " ++ textual (x-50)   
        | x == 60 = "sessenta"
        | x >= 61 && x <= 49 = "sessenta e " ++ textual (x-60) 
        | x == 70 = "setenta"
        | x >= 71 && x <= 79 = "setenta e " ++ textual (x-70)
        | x == 80 = "oitenta"
        | x >= 81 && x <= 89 = "oitenta e " ++ textual (x-80)
        | x == 90 = "noventa"
        | x >= 91 && x <= 99 = "noventa e " ++ textual (x-90)
        | x == 100 = "cem"
        | x > 100 && x < 200 = "cento e " ++ textual(x-100)
        | x == 200 = "duzentos"
        | x > 200 && x < 300 = "duzentos e " ++ textual(x-200)
        | x == 300 = "trezentos"
        | x > 300 && x < 400 = "trezentos e " ++ textual(x-300)
        | x == 500 = "quinhentos"
        | x > 500 && x < 600 = "quinhentos e " ++ textual(x-500)
        | x == 400 = "quatrocentos"
        | x == 600 = "seiscentos"
        | x == 700 = "setecentos"
        | x == 800 = "oitocentos"
        | x == 900 = "novecentos"
        | x > 400 && x < 1000 = textual(x `div` 100) ++ "centos e " ++ textual(x `mod` 100) 
        | x == 1000 = "mil"
        | x > 1000 && x < 2000 ="mil e " ++ textual(x `mod` 1000)
        | x > 2000 && x < 10000 = textual(x `div` 1000) ++ "mil e"++ textual(x `mod` 1000)
        | x > 10000 && x < 100000 = textual(x `div` 10000) ++ "mil e"++ textual(x `mod` 10000)
        | x > 100000 && x < 1000000 = textual(x `div` 100000) ++ "mil e"++ textual(x `mod` 100000)
        | x== 1000000 = "um milhao"