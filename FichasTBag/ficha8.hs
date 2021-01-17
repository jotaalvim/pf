module Ficha8 where

import Ficha3
import Ficha7
data Frac = F Integer Integer

l1 = [(F 7 8),(F 9 8),(F 7 9),(F 10 9)]


normaliza :: Frac -> Frac
normaliza (F a b) 
 | signum a == signum b && signum a == (-1) =  (F (div ((-1)*a) c) (div ((-1)*b) c))
 | signum a /= signum b && signum a == (1)  =  (F (div ((-1)*a) c) (div ((-1)*b) c))  
 | otherwise   =  (F (div a c) (div b c))
 where c = mdc a b 

mdc :: Integer -> Integer -> Integer
mdc m n
 | m == 0 = n
 | m > 0 = mdc (mod n m) m
 | m < 0 && n < 0 = mdc (-m) (-n)
 | m < 0 && n > 0 = (mdc (-m) n)
 | m > 0 && n < 0 = (mdc m (-n))

somaFrac :: Frac -> Frac -> Frac
somaFrac (F a b) (F a1 b1)
 | b == b1 = (F (a+a1) b)
 | otherwise = somaFrac (F (a*b1) (b*b1)) (F (a1*b) (b1*b))

multFrac :: Frac -> Frac -> Frac
multFrac (F a b) (F a1 b1) = (F (a*a1) (b*b1))

fromIntegerFrac :: Integer -> Frac
fromIntegerFrac n = (F n 1)

sinalFrac :: Frac -> Frac
sinalFrac (F a b) | signum a == signum b =  (F 1 1)
                  | signum a /= signum b =  (F (-1) 1)  
                  
instance Eq Frac where
    (F a b) == (F a1 b1) = a2 == a3 && b2 == b3
                     where (F a2 b2) = normaliza (F a b)
                           (F a3 b3) = normaliza (F a1 b1)

instance Ord Frac where
    compare (F a b) (F a1 b1) | (fromIntegral a)/(fromIntegral b) == (fromIntegral a1)/(fromIntegral b1) = EQ
                              | (fromIntegral a)/(fromIntegral b) <  (fromIntegral a1)/(fromIntegral b1) = LT
                              | otherwise = GT

instance Show Frac where
    show (F a b) = show a ++ "/" ++ show b

instance Num Frac where
    (+) = (somaFrac)
    (*) = (multFrac)
    abs (F a b) = (F (abs a) (abs b))
    signum = sinalFrac
    negate (F a b) = (F (negate a) b)
    fromInteger = fromIntegerFrac

seleciona :: Frac -> [Frac] -> [Frac]
seleciona f l = filter (>((fromIntegral 2)*f)) l

--Exercicio 2

instance Show ExpInt where 
    show = infixa

instance Eq ExpInt where
    a == b = calcula a == calcula b 

instance Num ExpInt where
    a + b = (Mais a b)
    a - b = (Menos a b)
    a * b = (Mult a b)
    abs (a) = if ((calcula a) < 0) then (Simetrico a) else a
    signum a = if ((calcula a) < 0) then (Const (-1)) else (Const 1)
    fromInteger a = (Const (fromIntegral a))

-- Exercicio 3

ext :: Extracto
ext = (Ext 300 [((D 1 9 2010),"LEV", Debito 60),((D 5 4 2010),"DEPOSITO", Credito 2000),((D 10 5 2010),"COMPRA", Debito 37.5),((D 22 1 2011),"ANUIDADE", Debito 8.0),((D 7 1 2011),"JUROS", Credito 100)])
{-
instance Ord Data where
    compare a b | a == b = EQ
                | anterior a b = LT
                | otherwise = GT

instance Show Data where
    show (D d m a) = show a ++ "/" ++ show m ++ "/" ++ show d
-}
ordena1 :: Extracto -> Extracto
ordena1 (Ext a l) = (Ext a (ordenaData l))

ordenaData :: [(Data, String, Movimento)] -> [(Data, String, Movimento)]
ordenaData [] = []
ordenaData (x:xs)
 | (map (\a -> 1) (filter (\a -> anterior (getData x) (getData a)) xs)) == [] = ordenaData xs ++ [x]
 | otherwise = ordenaData (xs ++ [x])
 where getData :: (Data, String, Movimento) -> Data
       getData (d,s,m) = d

instance Show Extracto where
    show (Ext s []) = "Saldo anterior: " ++ show s ++ "\n" ++ "---------------------------------------\nData Descricao Credito Debito\n---------------------------------------" 
    show (Ext sa ((d, s, Debito x):t)) = show d ++ "  " ++ s ++ "            " ++ show x ++ "\n" ++ show (Ext (saldo (Ext sa [(d, s, Debito x)])) t)
    show (Ext sa ((d, s, Credito x):t)) = show d ++ "  " ++ s ++ "     " ++ show x ++ "\n" ++ show (Ext (saldo (Ext sa [(d, s, Credito x)])) t)
    show (Ext sa [(d, s, Credito x)]) = show d ++ "  " ++ s ++ "     " ++ show x ++ "\n---------------------------------------\nSaldo actual: " ++ show (saldo (Ext sa [(d, s, Credito x)]))
    show (Ext sa [(d, s, Debito x)]) = show d ++ "  " ++ s ++ "            " ++ show x ++ "\n---------------------------------------\nSaldo actual: " ++ show (saldo (Ext sa [(d, s, Debito x)]))