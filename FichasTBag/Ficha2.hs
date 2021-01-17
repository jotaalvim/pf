import Data.Char

-- 2a)-------Dobro dos elementos de uma lista
dobros :: [Float] -> [Float]
dobros [] = []
dobros (x:xs) = 2*x : dobros xs

--2b)--------Calcular quantas vezes um caracter ocorre numa lista
numOcorre :: Char -> String -> Int
numOcorre _ "" = 0
numOcorre a (x:xs) | a == x = 1 + numOcorre a xs
                   | otherwise = numOcorre a xs

--2c)--------testa se uma lista tem só elementos positivos
positivos :: [Int] -> Bool
positivos [] = False
positivos [a] = a>0
positivos (x:xs) = x>0 && positivos xs

--2d)--------retira os elementos não positivos de uma lista
soPos :: [Int] -> [Int]
soPos [] = []
soPos (x:xs) |x>0 = x : soPos xs
             |otherwise = soPos xs
            
--2e)--------Soma todos os números negativos de uma lista
somaNeg :: [Int] -> Int
somaNeg [] = 0
somaNeg (x:xs) | x<0 = x + somaNeg xs
               | otherwise = somaNeg xs

--2f)---------dá os 3 ultimos elementos de uma lista
tresUlt :: [a] -> [a]
tresUlt (x:xs) | length (x:xs) <= 3 = (x:xs)
               | otherwise = tresUlt xs

--2g)--------segundos elementos dos pares
segundos :: [(a,b)] -> [b]
segundos [] = []
segundos ((a,b):xs) = b : segundos xs

--2h)-------testa se um elemento ocorre como o primeiro elemento de uma lista de duplos
nosPrimeiros :: Eq a => a -> [(a,b)] -> Bool
nosPrimeiros _ [] = False
nosPrimeiros a [x] = a == fst x
nosPrimeiros a (x:xs) = a == fst x || nosPrimeiros a xs

--2i)--------soma triplos
sumTriplos :: (Num a, Num b, Num c) => [(a,b,c)] -> (a,b,c)
sumTriplos [] = (0,0,0)
sumTriplos [x] = x
sumTriplos ((a,b,c):xs) = (a+x,b+y,c+z) where (x,y,z) = sumTriplos xs


--3a)----------seleciona de uma lista de caracteres os elementos que sejam algarismos
soDigitos :: [Char] -> [Char]
soDigitos [] = []
soDigitos (x:xs) | isDigit x = x : soDigitos xs
                 | otherwise = soDigitos xs 

--3b)-----------conta quantos caracteres de uma lista são minusculos
minusculos :: [Char] -> Int
minusculos [] = 0
minusculos (x:xs) | isLower x == True = 1 + minusculos xs
                  | otherwise = minusculos xs

--3c)-----------recebe uma lista e devolve os algarismos dessa lista por ordem
nums :: String -> [Int]
nums "" = []
nums (x:xs) | isDigit x = digitToInt x : nums xs
            | otherwise = nums xs

--4
type Polinomio = [Monomio]
type Monomio = (Float, Int)

p1 :: Polinomio
p1 = [(2,3), (3,4), (5,3), (4,5)]

conta :: Int -> Polinomio -> Int
conta n p = sum (map (\x -> if (snd x) == n then 1 else 0) p)

grau :: Polinomio -> Int
grau [] = 0
grau p = maximum (map (\x -> snd x) p)

selgrau :: Int -> Polinomio -> Polinomio
selgrau n p = filter (\x -> snd x == n) p

deriv :: Polinomio -> Polinomio
deriv p = map (\(c,e) -> (c*(fromIntegral e),e-1)) p

calcula :: Float -> Polinomio -> Float
calcula x p = sum (map (\(c,e) -> c*(x)^e) p)

simp :: Polinomio -> Polinomio
simp p = filter (\x -> fst x /= 0) p

mult :: Monomio -> Polinomio -> Polinomio
mult (cm,em) p = map (\(c,e) -> (c*cm, e+em)) p 

normaliza :: Polinomio -> Polinomio
normaliza [] = []
normaliza p@(m:rp) = ((junta (selgrau (snd m) p)) : normaliza (filter (\x -> snd x /= snd m) p))
                   where junta p = let (c,_) = unzip p
                                       e = grau p
                                   in (sum c, e)

soma :: Polinomio -> Polinomio -> Polinomio
soma p1 p2 = normaliza (p1++p2)

produto :: Polinomio -> Polinomio -> Polinomio
produto [] _ = []
produto _ [] = []
produto (m:rp) p = normaliza ((mult m p) ++ produto rp p)

ordena :: Polinomio -> Polinomio
ordena [] = []
ordena p = ordena (filter (\x -> snd x < grau p) p) ++ (normaliza(selgrau (grau p) p))

equiv :: Polinomio -> Polinomio -> Bool
equiv p1 p2 = (normaliza.ordena.simp) p1 == (normaliza.ordena.simp) p2