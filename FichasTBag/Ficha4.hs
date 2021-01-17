import Data.Char

--Ficha 4

--Exercicio 3
digitAlpha :: String -> (String, String)
digitAlpha s = ((filter (isAlpha) s), (filter (isDigit) s))

{-
digitAlpha2 :: String -> (String,String)
digitAlpha2 s = separa s ([],[])
 where separa :: String -> (String, String) -> (String,String)
       separa [] (ll,ld) = (ll,ld)
       separa (x:xs) (ll,ld)
        |isDigit x = separa xs (ll,ld ++ [x])
        |isAlpha x = separa xs (ll ++ [x],ld)
        |otherwise = separa xs (ll,ld)
-}

--Exercicio 4

nzp :: [Int] -> (Int,Int,Int)
nzp l = (sum $ map (\x -> if x<0 then 1 else 0) l, sum $ map (\x -> if x==0 then 1 else 0) l, sum $ map (\x -> if x>0 then 1 else 0) l)

{-   As que a stora fez na aula
nzp :: [Int] -> (Int,Int,Int)
nzp [] = (0,0,0)
nzp (x:xs)
 | x<0 = (nn+1,nz,np)
 | x==0 = (nn,nz+1,np)
 | x>0 = (nn,nz,np+1)
 where (nn,nz,np)=nzp xs

nzp2 :: [Int] -> (Int,Int,Int)
nzp2 l = conta l (0,0,0)
 where conta [] (nn,nz,np) = (nn,nz,np)
       conta (x:xs) (nn,nz,np) 
        | x<0 = conta xs (nn+1,nz,np)
        | x==0 = conta xs (nn,nz+1,np)
        | x>0 = conta xs (nn,nz,np+1)
-}

--Exercicio 5

divMod2 :: Integral a => a -> a -> (a, a)
divMod2 a b = (div a b, mod a b)

--Exercicio 6
fromDigits :: [Int] -> Int
fromDigits [] = 0
fromDigits (x:xs) = calcula (x:xs) (length xs)
 where calcula [x] _ = x
       calcula (y:ys) k = y*10^k + calcula ys (k-1)

fromDigits2 :: [Int] -> Int
fromDigits2 l = conv (reverse l)
 where conv :: [Int] -> Int
       conv [] = 0
       conv (x:xs) = x + 10*(conv xs)   

fromDigits3 :: [Int] -> Int
fromDigits3 l = conv l 0
 where conv [] s = s
       conv (x:xs) s = conv xs (x+10*s)


--7
maxSumInit :: (Num a, Ord a) => [a] -> a
maxSumInit l = aux l 0 0
  where aux :: (Num a, Ord a) => [a] -> a -> a -> a
        aux [] m s = m 
        aux (x:xs) m s 
          | (s + x) > m = aux xs (s+x) (s+x)
          | otherwise = aux xs m (s+x)


--8
fib :: Int -> Int
fib 0 = 0
fib n = fibAux n 0 1
  where fibAux 0 v1 v2 = v1
        fibAux 1 v1 v2 = v2
        fibAux n v1 v2 = fibAux (n-1) v2 (v1+v2)