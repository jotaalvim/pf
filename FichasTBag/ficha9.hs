
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