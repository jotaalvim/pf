import Data.List
import System.Random
myinsert :: Ord a => a -> [a] -> [a]

--insert 25 [1,20,30,40] = [1,20,25,30,40]

myinsert x [] = [x]
myinsert x (h:t) | x <= h  = x :(h:t)
                 | x > h = h: myinsert x t

catMaybes ::[Maybe a] -> [a]
catMaybes [] = []
catMaybes ((Just a):t) = a:catMaybes t
catMaybes ((Nothing):t) = catMaybes t

------ 3


data Exp a = Const a
           | Var String
           | Mais (Exp a) (Exp a)
           | Mult (Exp a) (Exp a) 


fo =  Mais (Var "d") (Mult (Const 3)(Const 3))

instance (Show a) => Show (Exp a) where 
    show (Var x)    = x
    show (Mais e d) = "("++(show e) ++ "+" ++ (show d)++")"
    show (Mult e d) = "("++(show e) ++ "x" ++ (show d)++")"
    show (Const a)  = show a

--sortOn fst [(3,1),(1,2),(2,5)] == [(1,2),(2,5),(3,1)]



mysortOn :: Ord b => (a -> b) -> [a] -> [a]
mysortOn f [] = []
mysortOn f (h:t) = inserir f h (mysortOn f t)

inserir :: Ord b => (a -> b ) -> a -> [a] -> [a]
inserir f a [] = [a]
inserir f x (h:t) | (f x) <= (f h)  = x :(h:t)
                  | (f x) > (f h) = h: inserir f x t


amplitude :: [Int] -> Int
amplitude [] = 0--       min  max l
amplitude l@(h:t) = m2 - m1
    where (m1,m2) = ampaux h h l


{-
amplitude l = m - mi
    where m  = maximum l
          mi = minimum l 
-}


-- devolve o min e o max da lista
ampaux :: Int -> Int -> [Int] -> (Int, Int)
ampaux m1 m2 [] = (m1, m2)
ampaux m1 m2 (h:t) = ampaux (min m1 h ) (max m2 h) t





-- dado uma lista ordenada 

parte :: [Int] -> ([Int],[Int])
parte l  = splitAt (i+1) listo
    where listo = (sort l)
          i = snd (last lo)
          lo = sort (sub listo)   

sub :: [Int] -> [(Int,Int)]
sub l = [ ((b-a),i) |(a,b,i) <- zip3 l (tail l) [0..]]






data Imagem = Quadrado Int
            | Mover (Int,Int) Imagem
            | Juntar [Imagem]


ex :: Imagem
ex = Mover (5,5) (Juntar [Mover (0,1) (Quadrado 5),Quadrado 4, Mover (4,3) (Quadrado 2)])



conta :: Imagem -> Int
conta (Quadrado x) = 1
conta (Mover _ i ) = conta i
conta (Juntar l  ) = map conta l  


{-

--inorder :: Imagem -> [Imagem]
--inorder ( (Quadrado ):t)

apaga :: Imagem -> IO Imagem
apaga Quadrado x = Juntar []
apaga Mover cord im = Mover cord (apago im )
apaga Juntar l = 
apaga i = do let k = conta i in 
                 x <- randomRIO (0,k)

-}

             
