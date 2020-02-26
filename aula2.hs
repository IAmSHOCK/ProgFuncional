segundo xs = head (tail xs)

trocar (x, y) = (y, x)

par x y = (x, y)

dobro x = 2*x

metade x = x/2

minuscula x = x>='a' && x<='z'

intervalo x a b = x >= a && x <= b

palindromo xs = reverse xs == xs

twice f x = f (f x)