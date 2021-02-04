
import System.Random

bingo :: IO ()
bingo = do putStrLn "Bem vindo ao Bingo"
           dialogoBingo []
           
dialogoBingo :: [Int] -> IO()
dialogoBingo l = do x <- randomRIO (1,90) ::IO Int
                    if length l == 90
                        then putStrLn "Acabou o programa"
                     else if elem x l
                     then 
                        do dialogoBingo l
                     else
                        do putStr ("Saiu o Número : ")
                           print x
                           a <- adicionaLista l x
                           putStrLn ""
                           dialogoBingo a

adicionaLista :: [Int] -> Int -> IO [Int]
adicionaLista l a = readIO (show (a : l))

{-
(b) mastermind :: IO () que implementa uma variante do jogo de descodificação de
padrões Mastermind. O programa deve começar por gerar uma sequência secreta
de 4 dı́gitos aleatórios que o jogador vai tentar descodificar. Sempre que o jogador
introduz uma sequência de 4 dı́gitos, o programa responde com o número de dı́gitos
com o valor correcto na posição correcta e com o número de dı́gitos com o valor
correcto na posição errada. O jogo termina quando o jogador acertar na sequência
de dı́gitos secreta.
-}

resultado :: [Int] ->  IO ()
resultado l = do b <- recebeNumero
                 c <- separaInt b
                 numeroCorretos <- verificaCorretosPosicaoCorreta l c
                 if numeroCorretos == 4 
                     then do putStrLn "\nAcertaste cabrão"
                     else do numeroCorretosPosErrada <- verificaCorretosPosicaoErrada l c
                             putStr "\nNúmero de dígitos na posição correta: "
                             print numeroCorretos 
                             putStr "Número de digitos corretos na posicao errada: "
                             print numeroCorretosPosErrada
                             resultado l

mastermind :: IO ()
mastermind = do a <- randomRIO (1000, 9999) :: IO Int
                b <- separaInt a
                if elem 0 b 
                    then do mastermind
                    else do resultado b

recebeNumero :: IO Int
recebeNumero = do putStrLn "\nDigite os 4 digitos"
                  x <- getLine
                  y <- readIO x
                  z <- separaInt y
                  if length z > 4 || length z < 4 
                    then do putStrLn "\nForam pedidos 4 digitos"
                            recebeNumero
                    else
                         do putStr "\nDigitou: "
                            print y
                            putStrLn ""
                            return y
                          
separaInt :: Int -> IO [Int]
separaInt x = readIO (show (separaInt1 x))

separaInt1 :: Int ->  [Int]
separaInt1 x 
 | x < 10 = [x]
 | otherwise = (separaInt1 (div x 10)) ++ [mod x 10]

verificaCorretosPosicaoCorreta :: [Int] -> [Int] -> IO Int
verificaCorretosPosicaoCorreta l1 l2 = readIO (show (verificaCorretosPosicaoCorreta1 l1 l2))

verificaCorretosPosicaoErrada :: [Int] -> [Int] -> IO Int
verificaCorretosPosicaoErrada l1 l2 = readIO (show (verificaCorretosPosicaoErrada1 l1 l2))

verificaCorretosPosicaoCorreta1 :: [Int] -> [Int] -> Int
verificaCorretosPosicaoCorreta1 [] [] = 0
verificaCorretosPosicaoCorreta1 [] _ = error "As listas devem ter o mesmo tamanho"
verificaCorretosPosicaoCorreta1 _ [] = error "As listas devem ter o mesmo tamanho"
verificaCorretosPosicaoCorreta1 (x:xs) (y:ys)
 | x == y = 1 + verificaCorretosPosicaoCorreta1 xs ys
 | otherwise = verificaCorretosPosicaoCorreta1 xs ys 

verificaCorretosPosicaoErrada1 :: [Int] -> [Int] -> Int
verificaCorretosPosicaoErrada1 _ [] = 0
verificaCorretosPosicaoErrada1 (x:xs) (y:ys)
 | elem y xs = 1 + verificaCorretosPosicaoErrada1 (xs ++ [x]) ys
 | otherwise = verificaCorretosPosicaoErrada1 (xs ++ [x]) ys 