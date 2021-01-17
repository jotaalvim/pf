module Ficha5 where

--EXERCICIO 1

--(a)
any' :: (a -> Bool) -> [a] -> Bool
any' p [] = False
any' p (x:xs)
  | p x = True
  | otherwise = any' p xs
 
any'' :: (a -> Bool) -> [a] -> Bool
any'' p [] = False
any'' p (x:xs) = p x || any'' p xs

--(b)
zipWith' :: (a->b->c) -> [a] -> [b] -> [c]
zipWith' f [] _ = []
zipWith' f _ [] = []
zipWith' f (x:xs) (y:ys) = (f x y) : zipWith' f xs ys

--(c)
takeWhile2 :: (a -> Bool) -> [a] -> [a]
takeWhile2 p (x:xs)
  | p x = x : takeWhile2 p xs
  | otherwise = []

--(d)
dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' _ [] = []
dropWhile' p (x:xs)
  | p x = dropWhile' p xs
  | otherwise = x:xs

--(e)
span' :: (a -> Bool) -> [a] -> ([a],[a])
span' _ [] = ([],[])
span' p (x:xs)
  | not (p x) = ([],(x:xs))
  | otherwise = let (e,d) = span p xs
                in (x:e, d) 

--(f)
deleteBy' :: (a -> a -> Bool) -> a -> [a] -> [a]
deleteBy' p x [] = []
deleteBy' p x (y:ys)
 | p x y = ys
 | otherwise = y : deleteBy' p x ys

--(g)
sortOn' :: Ord b => (a -> b) -> [a] -> [a]
sortOn' f [] = []
sortOn' f (x:xs) = insert x (sortOn' f xs)
    where insert x [] = [x]
          insert x (y:ys)
           | f x < f y = x : y : ys
           | otherwise = y : insert x ys 

--EXERCICIO 2

type Polinomio = [Monomio]
type Monomio = (Float, Int)

selgrau :: Int -> Polinomio -> Polinomio
selgrau g p = filter (\(c,e) -> e == g) p 

conta :: Int -> Polinomio -> Int
conta g p = sum (map (\(c,e) -> if e == g then 1 else 0) p )

conta' :: Int -> Polinomio -> Int
conta' g p = foldr aux 0 p
  where aux :: Monomio -> Int -> Int
        aux (c,e) n = if e == g then 1+n else n 

grau :: Polinomio -> Int
grau p = maximum (map snd p)

deriv :: Polinomio -> Polinomio
deriv p = filter (\(c,e) -> c /= 0) (map (\(c,e) -> (c*(fromIntegral e), (e-1))) p)

calcula :: Float -> Polinomio -> Float
calcula v p = sum (map (\(c,e) -> c * (v^e)) p)

mult :: Monomio -> Polinomio -> Polinomio
mult (c,e) p = map (\(c1,e1) -> (c1*c, e1+e)) p

ordena :: Polinomio -> Polinomio
ordena [] = []
ordena (m:p) = ordena (lmen) ++ [m] ++ ordena (lmai)
             where lmen = filter (\m1 -> snd m1 <= snd m) p
                   lmai = filter (\m1 -> snd m1 > snd m) p

ordena' :: Polinomio -> Polinomio
ordena' [] = []
ordena' p = sortOn' snd p

--EXERCICIO 3
type Mat a = [[a]]

m :: Mat Float
m = [[1,2,3], [1,2,3], [1,2,3]]

m1:: Mat Float
m1 = [[1,2,3], [0,4,5], [0,0,6]]


mostraLinha :: Show a => [a] -> String
mostraLinha [] = ""
mostraLinha [x] = show x
mostraLinha (x:xs) = show x ++ " " ++ mostraLinha xs

mostraMat :: Show a => Mat a -> String
mostraMat [] = []
mostraMat (x:xs) = ("| " ++ mostraLinha x ++ " |\n") ++ mostraMat xs

printMat :: Show a => Mat a -> IO()
printMat m = putStrLn $ mostraMat m

dimOK :: Mat a -> Bool
dimOK [l] = True
dimOK (l1:l2:m) = (length l1) == (length l2) && dimOK (l2:m)

transpose :: Mat a -> Mat a
transpose ([]:_) = []
transpose m = (map head m) : transpose (map tail m)

triSup :: (Eq a, Num a) => Mat a -> Bool
triSup [] = True
triSup (l:m) = (all (==0) (map head m)) && triSup (map tail m)

dimMat :: Mat a -> (Int,Int)
dimMat [] = (0,0)
dimMat m = (length $ head m, length m)

addMat :: Num a => Mat a -> Mat a -> Mat a
addMat [] [] = []
addMat m1 m2 
 | dimMat m1 /= dimMat m2 = error "As matrizes devem ter a mesma dimensao"   
 | otherwise  = (zipWith (+) (head m1) (head m2)) : addMat (tail m1) (tail m2)

multMat :: Num a => Mat a -> Mat a -> Mat a 
multMat [] _ = []
multMat (x:xs) m2
 | (snd.dimMat) m1 /= (fst.dimMat) m2 = error "Nao e possivel"
 | otherwise = ((map (sum)) $ multLinhaMat x m) : multMat xs m2 
 where m = transpose m2
       multLinhaMat l [] = []
       multLinhaMat l (x:xs) = (zipWith (*) l x):multLinhaMat l xs

zipWMat :: (a -> b -> c) -> Mat a -> Mat b -> Mat c
zipWMat f [] _ = []
zipWMat f _ [] = []
zipWMat f (x:xs) (y:ys) = (zipWith f x y) : zipWMat f xs ys

rotateLeft :: Mat a -> Mat a
rotateLeft [] = []
rotateLeft [[x1],[x2],[x3]] = [[x1,x2,x3]]
rotateLeft m = (map (last) m) : rotateLeft (map (init) m)
