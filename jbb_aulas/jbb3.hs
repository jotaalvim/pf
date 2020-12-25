
import System.Random
--(+) :: Int -> Int -> Int -- muito restrita
--
--(+) :: a -> a -> a
--
--
--'a' :: Char
--
--(+) :: Num a => a -> a -> a
--    (tipo da função) a->a->a
--    (tipo do tipo a) a é uma instância da classe Num--a :: Num
--
--Classes em Haskel
--
-----------------------------------------------------
--1. Defenir uma classe

--class NOME a where    -- a classe NOME é constituidas pelos tipos a em que...
    -- restrições: funções que têm que existir

--class Eq a where
--    (==)  :: a -> a -> Bool
--    (/=)  :: a -> a -> Bool

--2. Defenir instancias
--instance CLASSE Tipo where -- Defenir Tipo como um elemento de CLASSE 

data BTree a = Empty | Node a (BTree a) (BTree a) 

-- fazer com que a Btree a seja uma instância de Eq
instance Eq a => Eq (BTree a) where
    -- defenir as funções (==) e (/=) com os tipos respetivos
    --(==) :: BTree a -> BTree a -> Bool
    Empty == Empty = True
    (Node x y z) == (Node p q r) = (x == p)
                                && (y == q)
                                && (z == r)
    --(/=) :: BTree a -> BTree a -> Bool
    a1 /= a2 = not (a1 == a2)

x, y :: BTree Int 
x = Empty
y = Node 3 Empty Empty 



dialogo :: IO ()
dialogo = do putStr "Nome? "
             x <- getLine
             putStr( "boa tarde " ++ x ++ "\n")




randomList :: Int -> (Int,Int) -> IO [Int]
randomList 0 (i,s) = return [] 
randomList n (i,s) = do x  <- randomRIO (i,s)
                        xs <- randomList (n-1) (i,s)
                        return (x:xs)
 
