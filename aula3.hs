-- ex 22
somaint:: Int
somaint = soma [x^2| x <-[1..100]]

soma:: [Int] -> Int
soma [] = 0
soma (h:t) = h + soma t

-- ex 23
aprox:: Int -> Double
aprox n = sum[((-1)^x)/((x+1)^2) | x<-[1..n]]