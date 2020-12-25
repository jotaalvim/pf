import System.Random


bingo :: IO ()
bingo = do { l <- geraLista 90 [0..90];
             apresenta l }

geraLista  :: Int -> [Int] -> IO [Int]
geraLista 0 _ = return []
geraLista n l = do p <- randomRIO (0,n-1)
                   let (l1,x:l2) = splitAt p l
                   xs <- geraLista (n-1) (l1++l2)
                   return (x:xs)

apresenta :: [Int] -> IO()
apresenta [] = putStrLn "FIM"
apresenta l@(x:xs) = do putStrLn "prima enter"
                        print l
                        getChar--getCh
                        print x
                        apresenta xs


-- é um getChar mas não faz echo do carater
{-
getCh :: IO Char
getCh = do hSetEcho stdin False 
           x <- getChar
           hSetEcho stdin True
           return x
-}






data Aposta = Ap [Int] (Int,Int)

valida :: Aposta -> Bool
valida (Ap l (a,b)) = length l ==  5 && numValidos l 
                      && a /= b && elem a [0..9]
                      && elem b [0..9]

numValidos :: [Int] -> Bool
numValidos [] = True
numValidos (h:t) = elem h [1..50] && not (elem h t) &&numValidos t 


comuns :: Aposta -> Aposta -> (Int,Int)
comuns (Ap l (a,b)) (Ap l2 (c,d)) = (numComs l l2, numComs [a,b] [c,d])


numComs :: [Int] -> [Int] -> Int
numCOms [] l = 0
numComs (x:xs) l = if elem x l then 1 +numComs xs l
                   else numComs xs l

instance Eq Aposta where
    ap1 == ap2 = comuns ap1 ap2 == (5,2) 


premio :: Aposta -> Aposta -> Maybe Int
premio ap ch = case (comuns ap ch) of 
                      (5,2) -> Just 1
                      (5,1) -> Just 2
                      (5,0) -> Just 3
                      (4,2) -> Just 4
                      (4,1) -> Just 5
                      (4,0) -> Just 6
                      (3,2) -> Just 7
                      (2,2) -> Just 8
                      (3,1) -> Just 9
                      (3,0) -> Just 10
                      (1,2) -> Just 11
                      (2,1) -> Just 12
                      (2,0) -> Just 13

leAposta :: IO Aposta
leAposta = do putStrLn "lista de números de 1..50"
              num <- getLine
              putStrLn "par de estrelas de 1..9"
              est <- getLine
              let ap = Ap (read num) (read est)
              if valida ap 
              then return ap
              else do putStrLn "invalido"
                      leAposta

joga :: Aposta -> IO ()
joga ch = do ap <- leAposta
             case (premio ap ch) of
                 Just n -> putStrLn ("tem premio "++ (show  FIXME SHOWPREMIO?))
                 Nothing -> putStrLn "nao tem premio" 

geraChave :: IO Aposta
geraChave = do l <- (geraLista 50 [1..50])
               le <- geraLista 9 [1..9]
               let ns = take 5 l 
                   [a,b] = take 2 le
               return (Ap ns (a,b))
