import Data.List
import Data.Char
--1 a
myunlines :: [String] -> String
myunlines []    = []
myunlines [a]   = a
myunlines (h:t) = h ++ "\n" ++ myunlines t


-- b

p2 :: (Eq a) => [a] -> [a] -> [a]
p2 (h:t) r = if elem h r then p2 t (delete h r)
             else h:p2 t r 
p2 x _ = x


-- 2


s = Inicio 3 (Fim (Inicio 3 Nil) 5) -- > 3 3 5
s2 = Fim (Inicio 3 Nil) 9

data Seq a = Nil | Inicio a (Seq a) | Fim (Seq a) a deriving (Show)

primeiro :: Seq a -> a
primeiro (Inicio x _ ) = x
primeiro (Fim Nil x) = x
primeiro (Fim e x) = primeiro e 

semUltimo :: Seq a -> Seq a
semUltimo Nil = Nil
semUltimo (Fim Nil a) = Nil 
semUltimo (Fim b  a  ) = b
semUltimo (Inicio a x) = Inicio a (semUltimo x) 

-- 3

arv = Node 3 (Node 1 Empty Empty) (Node 5 Empty Empty)
data BTree a = Empty | Node a (BTree a) (BTree a) deriving (Show)

prune :: Int -> BTree a -> BTree a
prune 0 (Node x e d) = (Node x Empty Empty)
prune n (Node x e d) = Node x (prune (n-1) e) (prune (n-1) d)


semMinimo :: (Ord a) => BTree a -> BTree a
semMinimo (Node x Empty d) = d
semMinimo (Node x e d) = Node x (semMinimo e) d  


-- 4
type Tabuleiro = [String]


tab :: Tabuleiro
tab = ["..R.",
       "R...",
       ".R..",
       "...R"]


posicoes :: Tabuleiro -> [(Int,Int)] 

posicoes tab = [ (x,y) | x <- [0..n], y <- [0..n], retira (x,y) tab  == 'R']
    where n = length tab -1

retira :: (Int,Int) -> Tabuleiro -> Char
retira (x,y) tab = tab !! y !! x







--valido :: Tabuleiro -> Bool
--valido tb = tv tab && th tab



th tab =  all (== True) [ (length (filter (=='R') x)) == 1 | x <- tab ]

transposta :: Tabuleiro -> Tabuleiro
transposta tab = [ map (!! k ) tab | k <- [0..length tab-1] ]

tv tab = all (== True) [ (length (filter (=='R') x)) == 1 | x <- (transposta tab) ]



--tv l = if length [ x | x <- (map head l), x =='R'] == 1 then True else False

--th :: Tabuleiro -> Bool
--th l = [[ x | x <- li, x == 'R' ]  | li <- l]









