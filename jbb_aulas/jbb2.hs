--data types de uma lista 
{-
data Lista a = Vazia | N a (Lista a) deriving Show

comp :: Lista a -> Int 
comp Vazia    = 0
comp (N x xs) = 1 + comp xs

-- 1 2 3 representada por N 1 (N 2 ( N 3 Vazia)):: Lista Int

-}
data ABin a = Vazia 
            | N a (ABin a) (ABin a) deriving Show

--exemplos 
a1, a2, a3, a4:: ABin Int

a1 = Vazia 
a2 = N 0 Vazia Vazia
a3 = N 5 (N 6 Vazia Vazia) 
         (N 7 Vazia Vazia)
a4 = N 100 a3 a2

comp :: ABin a -> Int
comp Vazia     = 0
comp (N n x y) = 1 + comp x + comp y --n::a ,y::ABin a,z:: ABin


altura :: ABin a -> Int 
altura Vazia     = 0 
altura (N r e d) = 1 +  max (altura e) (altura d) 

mapABin :: ( a -> b ) -> ABin a -> ABin b
mapABin f Vazia = Vazia
mapABin f (N r e d) = N (f r) (mapABin f e)  (mapABin f d)


elem' :: Eq a => a -> [a] -> Bool
elem' x [] = False 
elem' x (h:t) | x == h = True 
              | otherwise = elem' x t

elemO :: Ord a => a -> [a] ->Bool
--recebe uma lista ordenada
elemO x [] = False
elemO x (h:t) | x == h = True
              | x < h  = False
              | otherwise = elemO x t

elemA :: Eq a => a -> ABin a-> Bool
elemA x Vazia = False
elemA x (N r e d) | x == r = True
                  | otherwise = (elemA x e) || (elemA x d) 

-- árvores binárias de procura
-- elementos à esquerda são todos menores ou iguais à raiz
-- elementos à direita sãp todos maiores ou iguais à raiz

procura :: Ord a => a -> ABin a -> Bool
--recebbe uma árvore de procura
procura x Vazia = False 
procura x (N r e d) | x == r = True
                    | x  < r = procura x e
                    | x  > r = procura x d 

acrescenta :: Ord a => a -> ABin a -> ABin a
-- recebe uma árvore de procura
-- retonra uma árvore de procura

acrescenta x Vazia = N x Vazia Vazia
acrescenta x (N r e d) | x <= r =N r (acrescenta x e) d
                       | otherwise = N r e (acrescenta x d)


fromList :: Ord a => [a] -> ABin a
--fromList [] = Vazia
--fromList (h:t) = acrescenta h (fromList t)
fromList = foldr (acrescenta) Vazia l


maior :: ABin a -> a
maior (N r e Vazia) = r
maior (N r e d) = maior d



