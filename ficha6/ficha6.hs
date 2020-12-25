data BTree a = Empty | Node a (BTree a) (BTree a) deriving Show

arv1 = Node 3 (Node 9 Empty Empty) (Node 2 Empty (Node 7 Empty Empty))


altura :: BTree a -> Int
altura Empty = 0
altura (Node x e d) = 1 + max (altura e) (altura d)

contaNodos :: BTree a -> Int
contaNodos Empty = 0
contaNodos (Node x e d) = 1 + (contaNodos e) + (contaNodos d)

folhas :: BTree a -> Int
folhas Empty = 0
folhas (Node x Empty Empty)  = 1 
folhas (Node x e d) = (folhas e) + (folhas d)

prune :: Int -> BTree a -> BTree a
prune 0 _  = Empty
prune n Empty = Empty
prune n (Node x e d) = Node x (prune (n-1) e) (prune (n-1) d)

path :: [Bool] -> BTree a -> [a] --(False corresponde a esquerda e True a direita)
path [] (Node x e d) = [x]
path _ Empty = []
path (h:t) (Node x e d) = if h then x: path t d 
                               else x: path t e

mirror :: BTree a -> BTree a
mirror Empty = Empty
mirror (Node x e d) = Node x (mirror d) (mirror e)

zipWithBT :: (a -> b -> c) -> BTree a -> BTree b -> BTree c
zipWithBT f (Node x e d) (Node x2 e2 d2) = Node (f x x2) (zipWithBT f e e2)(zipWithBT f d d2) 
zipWithBT f Empty Empty = Empty 

t = (Node (1,2,3) (Node (10,20,30) Empty Empty) (Node (100,200,300) Empty Empty))

unzipBT :: BTree (a,b,c) -> (BTree a,BTree b,BTree c)
unzipBT (Node (a,b,c) e d) = (Node a e1 d1 ,Node b e2 d2,Node c e3 d3)
    where (e1,e2,e3) = unzipBT e
          (d1,d2,d3) = unzipBT d
unzipBT Empty =(Empty,Empty,Empty) 

-- 2


-- arvores b. de procura estão ordenadas, neste caso temos que ir sempre para o ramo da esquerda

minimo :: BTree a -> a
minimo (Node r Empty _ ) = r
minimo (Node r e _) = minimo e

-- minimo para qualquer arvore binária

m :: Ord a => BTree a -> a
m (Node r Empty Empty) = r
m (Node r Empty d) = min r (m d)
m (Node r e Empty) = min r (m e)
m (Node r e d) = min r (min (m e) (m d))



minMaybe :: Ord a => Maybe a -> Maybe a -> Maybe a
minMaybe x Nothing = x
minMaybe Nothing x = x
minMaybe (Just x1) (Just x2) = Just (min x1 x2) 

m2 :: Ord a => BTree a -> Maybe a
m2 Empty = Nothing
m2 (Node r e d) = minMaybe (Just r) (minMaybe (m2 e) (m2 d))




semMinimo :: Ord a => BTree a -> BTree a
semMinimo (Node r Empty d) = d
semMinimo (Node r e d) = (Node r (semMinimo e) d)  


minSmin :: Ord a => BTree a -> (a,BTree a)
minSmin (Node r Empty d) = (r,d)
minSmin (Node r e d) = (x, Node r e' d)
    where (x,e') = minSmin e

remove :: Ord a => a -> BTree a -> BTree a
remove x Empty = Empty
remove x (Node r e d) 
     | x == r    = Node m e a 
     | otherwise = if x < r then Node r (remove x e) d 
                            else Node r e (remove x d)
     where (m,a) = minSmin d







