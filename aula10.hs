data Arv a = Vazia | No a (Arv a) (Arv a) deriving Show

-- ARVORES AVL
tamanho :: Arv a -> Int
tamanho Vazia = 0
tamanho (No v esq dir) = 1 + (max (tamanho esq) (tamanho dir)) 

altura :: Arv a -> Int
altura Vazia = 0
altura (No _ esq dir) = 1 + max (altura esq) (altura dir)

desvio :: Arv a -> Int
desvio Vazia = 0
desvio (No _ esq dir) = altura esq - altura dir

pesquisaAVL :: Ord a => a -> Arv a -> Bool
pesquisaAVL x Vazia = False
pesquisaAVL x (No y esq dir)
    | x==y = True
    | x<y = pesquisaAVL x esq
    | x>y = pesquisaAVL x dir

rodar_dir :: Arv a -> Arv a
rodar_dir (No x (No y t1 t2) t3) = No y t1 (No x t2 t3)
rodar_dir t = t -- identidade nos outros casos

rodar_esq :: Arv a -> Arv a
rodar_esq (No x t1 (No y t2 t3)) = No y (No x t1 t2) t3
rodar_esq t = t 


corrige_dir :: Arv a -> Arv a
corrige_dir (No x t1 t2)
    | desvio t1 == -1 = rodar_dir (No x (rodar_esq t1) t2)
    | otherwise = rodar_dir (No x t1 t2)
corrige_dir t = t -- identidade noutros casos

corrige_esq :: Arv a -> Arv a
corrige_esq (No x t1 t2)
    | desvio t2 == 1 = rodar_esq (No x t1 (rodar_dir t2))
    | otherwise = rodar_esq (No x t1 t2)
corrige_esq t = t

re_equilibrar :: Arv a -> Arv a
re_equilibrar t
    | d== 2 = corrige_dir t
    | d== -2 = corrige_esq t
    | otherwise = t
    where d = desvio t

inserirAVL :: Ord a => a -> Arv a -> Arv a
inserirAVL x Vazia = No x Vazia Vazia
inserirAVL x (No y esq dir)
    | x==y -- j´a ocorre
        = No y esq dir
    | x<y -- inserir `a esquerda
        = re_equilibrar (No y (inserirAVL x esq) dir)
    | x>y -- inserir `a direita
        = re_equilibrar (No y esq (inserirAVL x dir))
-- Fim de ARVORES AVL

testeArv:: Arv Int
testeArv = No 8 (No 5 (No 3 Vazia Vazia) Vazia) (No 10 (No 9 Vazia Vazia) (No 15 Vazia Vazia))

--ex 70
contaArv:: Arv a -> Int
contaArv Vazia = 0
contaArv (No v esq dir) = 1 + (contaArv esq) + (contaArv dir)

sumArv :: Num a => Arv a -> a
sumArv Vazia = 0
sumArv (No v esq dir) = v + (sumArv esq) + (sumArv dir)

--ex 71
listar :: Arv a -> [a]
listar Vazia = []
listar (No v esq dir) = (listar dir) ++ [v] ++ (listar esq)

--ex 72
nivel :: Int -> Arv a -> [a]
nivel _ Vazia = []
nivel 0 (No v esq dir) = [v]
nivel n (No v esq dir) = (nivel  (n-1) esq) ++ (nivel (n-1) dir)

--ex 73
construir :: [a] -> Arv a
construir [] = Vazia
construir xs = No x (construir xs') (construir xs'')
            where n = length xs `div` 2 -- ponto m´edio
                  xs' = take n xs -- valores `a esquerda
                  x:xs'' = drop n xs -- valores central e `a direita

inserir :: Ord a => a -> Arv a -> Arv a
inserir x Vazia = No x Vazia Vazia
inserir x (No y esq dir)
    | x==y = No y esq dir -- j´a ocorre
    | x<y = No y (inserir x esq) dir -- insere `a esquerda
    | x>y = No y esq (inserir x dir ) -- insere `a direit

--ex 74
-- mapRec :: (a -> b) ->  [a] ->  [b]
-- mapRec f [] = 
-- mapRec f (x:xs) = 

mapArv :: (a -> b) -> Arv a -> Arv b
mapArv f Vazia = Vazia
mapArv f (No v esq dir) = (No (f v) (mapArv f esq) (mapArv f dir))

--ex 75
foldArv :: (a -> b -> b -> b) -> b -> Arv a -> b
foldArv f v Vazia = v
foldArv f v (No x esq dir) = f x (foldArv f v esq) (foldArv f v dir)

