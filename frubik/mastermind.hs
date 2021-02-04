import System.Random




--master :: IO ()


data Sinal = Sim | Nao deriving (Show,Eq)

main :: IO()
main = do
    p <- geralista
    (master p)

master :: [Int] -> IO()
master x = do
    li <- pega
    let c = compara x li
    putStrLn $ show c
    if all (==Sim) c 
        then putStrLn "Parabens Ganhaste"
    else do master x



geralista :: IO [Int]
geralista = do 
    x  <- randomRIO(0,9)
    x2 <- randomRIO(0,9)
    x3 <- randomRIO(0,9)
    x4 <- randomRIO(0,9)
    return [x,x2,x3,x4]


compara :: [Int] -> [Int] -> [Sinal]
compara []    []      = []
compara (h:t) (h2:t2) = if h == h2 
                        then Sim : compara t t2
                        else Nao : compara t t2


pega :: IO [Int]
pega = do 
    putStrLn "Dá-me 4 números separados por espaços"
    linha <- getLine
    return $ map read $ words linha







