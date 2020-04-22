--ex 64
elefantes :: Int -> IO ()
elefantes x = repete 2 x

repete :: Int -> Int -> IO ()
repete v n |(v == n) = return()
           |otherwise = do putStrLn("Se " ++ show(v) ++ " elefantes  incomodam muita gente,")
                           putStrLn(show(v+1) ++ " elefantes  incomodam muito mais!")
                           repete (v+1) n

elefantes2 :: Int -> IO ()
elefantes2 n = sequence_ [putStrLn("Se " ++ show(v) ++ " elefantes  incomodam muita gente," ++ show(v+1) ++ " elefantes  incomodam muito mais!") | v <- [2..n-1]]

--ex 65
wc :: String -> IO()
wc file = do {
                str <- readFile file;
                putStr (show (length (lines str)) ++ "    ");
                putStr (show (length (words str)) ++ "    ");
                putStr (show (length (str)) ++ "   ");
                putStrLn (file);
            }
--ex 66
reverseLines :: IO ()
reverseLines = do {
                        putStrLn("Inserir string");
                        str <- getLine;
                        if str == "Stop" then return() 
                            else do {
                                        putStrLn(reverse str); 
                                        reverseLines;
                                    }  
                  }


--ex 67
-- rot13 :: String -> IO ()
-- rot13 file = do {
--                     str <- getLine;

--                 }

-- succ13 :: Char ->       

--ex 68
adivinha :: String -> IO ()
adivinha secret = do {
                        putStrLn(mask secret);
                        jogo secret(mask secret) 1;
                    }


mask :: String -> String
mask l = take (length l) (repeat ('*'))
-- mask l = map (\v -> '*') l

jogo :: String -> String -> Int -> IO()
jogo secret display n = do putStrLn("Adivinhe letra")
                           x <- getLine
                           let c = head x
                           let d = reveal secret display c
                           if (elem c secret) then putStrLn d
                           else putStrLn("Não ocorre!")
                           if (secret == d) then do {putStrLn ("Adivinhou em " ++ show n ++ " tentativas."); return();}
                           else jogo secret d  (n+1)   

reveal :: String -> String -> Char -> String
reveal [] d _ = d
reveal (x:xs) (y:ys) c | (c==x) = do c:(reveal xs ys c)
                       | otherwise = y:(reveal xs ys c)