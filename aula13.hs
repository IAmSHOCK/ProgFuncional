data Arv a = No a (Arv a) (Arv a)
           | Vazia deriving Show


altura :: Arv a -> Int
altura Vazia = 0
altura (No v e d) = 1 + max ((altura e) (altura d))

desvio :: Arv a -> Int
desvio Vazia = 0
desvio (No _ e d) = altura e - altura d