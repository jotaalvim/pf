module Ficha1 where

import Data.Char    

--Exercicio1

perimetrococo :: Float -> Float
perimetrococo r = 2*pi*r

dist :: (Double,Double) -> (Double,Double) -> Double
dist (x1,y1) (x2,y2) = sqrt ( (x2-x1)^2 + (y2-y1)^2 )

primUlt :: [a] -> (a,a)
primUlt [] = error "Impossível"
primUlt l  = (head l, last l)

multiplo :: Int -> Int -> Bool
multiplo m n 
 | mod m n == 0 = True
 | otherwise = False

truncaImpar :: [a] -> [a]
truncaImpar l = if even (length l) then l else tail l 

max2 :: Int -> Int -> Int
max2 a b = if a > b then a else b

max3 :: Int -> Int -> Int -> Int
max3 a b c = max2 c (max2 a b)

--Exercicio2

nRaizes :: Float -> Float -> Float -> Int
nRaizes a b c 
 | f >  0 = 2
 | f == 0 = 1
 | f <  0 = 0
 where f = b^2 - 4*a*c

raizes :: Float -> Float -> Float -> [Float]
raizes a b c 
 | (nRaizes a b c) >  1 = [((-b) - f)/2*a,((-b) + f)/2*a]
 | (nRaizes a b c) == 1 = [(-b)/2*a]
 | otherwise = []
 where f = sqrt(b^2 - 4*a*c)

--Exercicio3
type Hora = (Int,Int)

horaValida :: Hora -> Bool
horaValida (x,y) = x<=24 && x>=0 && y<60 && y>=0

horaPosterior :: Hora -> Hora -> Bool
horaPosterior (x1,y1) (x2,y2) = x1>=x2 && y1>y2

horaToMin :: Hora -> Int
horaToMin (x,y) = x*60 + y

minToHora :: Int -> Hora
minToHora m = (div m 60, mod m 60)

difHoras :: Hora -> Hora -> Int
difHoras h1@(x1,y1) h2@(x2,y2)
 | f < 0 = (-f)
 | otherwise = f
 where f = (horaToMin h1) - (horaToMin h2)

addMin :: Hora -> Int -> Hora
addMin (h,m) m1 = minToHora (horaToMin (h,m) + m1)

--Exercicio4

data Hora1 = H Int Int deriving (Show,Eq)

horaValida2 :: Hora1 -> Bool
horaValida2 (H h m) = h<=24 && h>=0 && m<60 && m>=0

horaPosterior2 :: Hora1 -> Hora1 -> Bool
horaPosterior2 (H h1 m1) (H h2 m2) = if h1 == h2 then h1 > h2 else h1 > h2

horaToMin2 :: Hora1 -> Int
horaToMin2 (H h m) = h*60 + m

minToHora2 :: Int -> Hora1
minToHora2 m = (H (div m 60) (mod m 60))

difHoras2 :: Hora1 -> Hora1 -> Int
difHoras2 h1 h2
 | f < 0 = (-f)
 | otherwise = f
 where f = (horaToMin2 h1) - (horaToMin2 h2)

addMin2 :: Hora1 -> Int -> Hora1
addMin2 h m = minToHora2 (horaToMin2 h + m)

--Exercicio5

data Semaforo = Verde | Amarelo | Vermelho deriving (Show,Eq)

next :: Semaforo -> Semaforo
next c 
 | c == Verde = Amarelo
 | c == Amarelo = Vermelho
 | otherwise = Verde

stop :: Semaforo -> Bool 
stop Vermelho = True
stop c = False

safe :: Semaforo -> Semaforo -> Bool
safe a b
 | ((a == Verde || a == Amarelo) && (b == Vermelho)) || ((b == Verde || b == Amarelo) && (a == Vermelho)) = True
 | otherwise = False

--Exercicio 6

data Ponto = Cartesiano Double Double | Polar Double Double deriving (Show,Eq)

posx :: Ponto -> Double
posx (Cartesiano x y) = x
posx (Polar d a) = d * (cos a)

posy :: Ponto -> Double
posy (Cartesiano x y) = y
posy (Polar d a) = d * (sin a)

raio :: Ponto -> Double
raio (Polar d a) = d
raio (Cartesiano x y) = sqrt ( x^2 + y^2)

angulo :: Ponto -> Double
angulo (Polar d a) = a
angulo (Cartesiano x y) = atan (x/y)

distpontos :: Ponto -> Ponto -> Double 
distpontos p1 p2 = sqrt((posx p2 - posx p1)^2 + (posy p2 - posy p1)^2)

-- Exercicio 7

data Figura = Circulo Ponto Double
            | Retangulo Ponto Ponto
            | Triangulo Ponto Ponto Ponto
              deriving (Show, Eq)

poligono :: Figura -> Bool
poligono (Circulo p r) = if r <= 0 then False else True 
poligono (Retangulo p1 p2) = if posx p1 == posx p2 || posy p1 == posy p2 then False else True
poligono (Triangulo p1 p2 p3) = (a < b + c && b < a + c && c < a + b)
                       where a = distpontos p1 p2
                             b = distpontos p2 p3
                             c = distpontos p1 p3

vertices :: Figura -> [Ponto]
vertices (Circulo _ _) = []
vertices (Retangulo p1 p2) = [p1, (Cartesiano (posx p2) (posy p1)), p2, (Cartesiano (posx p1) (posy p2))]
vertices (Triangulo p1 p2 p3) = [p1,p2,p3]

area :: Figura -> Double
area (Triangulo p1 p2 p3) =
 let a = distpontos p1 p2
     b = distpontos p2 p3
     c = distpontos p3 p1
     s = (a+b+c) / 2 -- semi-perimetro
 in  sqrt (s*(s-a)*(s-b)*(s-c))
area (Circulo p r) = pi*(r^2)
area (Retangulo p1 p2) = (abs ((posx p2) - (posx p1))) * (abs ((posy p2) - (posy p1)))

perimetro :: Figura -> Double
perimetro (Circulo p r) = 2*pi*r
perimetro (Retangulo p1 p2) = 2*(abs ((posx p2) - (posx p1))) + 2*(abs ((posy p2) - (posy p1)))
perimetro (Triangulo p1 p2 p3) = (distpontos p1 p2) + (distpontos p2 p3) + (distpontos p1 p3)

-- Exercicio 8

myIsLower :: Char -> Bool
myIsLower c = if ord c <= 122 && ord c >= 97 then True else False

myIsDigit :: Char -> Bool
myIsDigit c = if ord c <= 57 && ord c >= 48 then True else False

myIsAlpha :: Char -> Bool
myIsAlpha c
 | ord c <= 122 && ord c >= 97 = True
 | ord c <= 90 && ord c >= 65 = True
 | otherwise = False

myToUpper :: Char -> Char
myToUpper c
 | myIsLower c = chr ((ord c) - 32)
 | otherwise = c

myIntToDigit :: Int -> Char
myIntToDigit i
 | i <= 9 = chr (i + 48)
 | otherwise = error (show i ++ " is not a Digit")

myDigitToInt :: Char -> Int
myDigitToInt c 
 | myIsDigit c = (ord c) - 48
 | otherwise = error (show c ++ " is not a Digit")