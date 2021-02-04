data BTree a = Empty | Node a (BTree a) (BTree a) deriving Show

arv1 = Node 3 (Node 9 Empty Empty) (Node 2 Empty (Node 7 Empty Empty))
arv2 = Node 3 (Node 1 Empty Empty) (Node 5 (Node 4 Empty Empty)(Node 6 Empty Empty))

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
unzipBT Empty =(Empty,Empty,Empty) 
unzipBT (Node (a,b,c) e d) = (Node a e1 d1 ,Node b e2 d2,Node c e3 d3)
    where (e1,e2,e3) = unzipBT e
          (d1,d2,d3) = unzipBT d

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

-- dado uma lista
minSmin ::  BTree a -> (a,BTree a)
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

--- 3

type Aluno = (Numero,Nome,Regime,Classificacao)
type Numero = Int
type Nome = String
data Regime = ORD | TE | MEL deriving Show
data Classificacao = Aprov Int | Rep | Faltou deriving Show
type Turma = BTree Aluno -- árvore binária de procura (ordenada por número)

a :: Aluno
a = (1,"Manel", ORD, Aprov 17)
a2 = (4,"Manela", ORD,Rep)
ar = Node a2 (Node a Empty Empty) Empty


-- atravessa  lista de forma inorder
inorder :: BTree a -> [a]
inorder Empty = []
inorder (Node x e d) = inorder e ++ [x] ++ inorder d  

-- a 

inscNum  :: Numero -> Turma -> Bool
inscNum x turma = any (==x) [ n | (n,_,_,_) <- inorder turma ]
 
inscNum2  :: Numero -> Turma -> Bool
inscNum2 x turma = foldr (\(n,_,_,_) a -> (x==n) || a ) False (inorder turma) 

inscNum3 :: Numero -> Turma -> Bool
inscNum3 x Empty = False
inscNum3 x (Node (n,_,_,_) e d ) | x == n = True
                                 | x > n  = inscNum3 x d
                                 | x < n  = inscNum3 x e

-- b 

inscNome :: Nome -> Turma -> Bool
inscNome x turma = any (==x) [ n | (_,n,_,_) <- inorder turma ]
 
inscNome2 :: Nome -> Turma -> Bool
inscNome2 x turma = foldr (\(_,n,_,_) a -> (x==n) || a ) False (inorder turma) 

inscNome3 :: Nome -> Turma -> Bool
inscNome3 x Empty = False
inscNome3 x (Node (_,nome,_,_) e d ) | x == nome = True
                                     | otherwise = inscNome3 x d || inscNome3 x e

-- c 

trabEst :: Turma -> [(Numero,Nome)]
trabEst turma = [ (n,nome) | (n,nome,_,_) <- inorder turma]


trabEst2 ::Turma -> [(Numero,Nome)]
trabEst2 Empty = []
trabEst2 a@(Node x e d) = (n,nome):trabEst2 q
    where ((n,nome,_,_),q) = minSmin a


-- d

nota :: Numero -> Turma -> Maybe Classificacao
nota n turma = if null k then Nothing else Just (head k)
    where k = [c | (nu,_,_,c) <- inorder turma, n == nu] 
          
-- e 


cf :: Turma -> Float-- cf = conta faltas
cf Empty = 0 
cf (Node (_,_,_,Faltou) e d) = 1 + cf e + cf d
cf (Node r e d) = cf e + cf d  

percFaltas2 :: Turma -> Float
percFaltas2 turma = 100 *cf turma /fromIntegral (length ( trabEst2 turma))

-- f

mediaAprov :: Turma -> Float
mediaAprov turma = fromIntegral notas / fromIntegral n
    where (n,notas) = foldr (\(_,_,_,Aprov x) (z,y) -> (1+z,x+y)) (0,0) (f k)
          k = inorder turma
          f (h@(_,_,_,Aprov x):t) = h : f t
          f (_:t) = f t
          f [] = [] 
-- ou 


-- conta notas  (nºalunos,somadas notas)
cn :: Turma -> (Int,Int)
cn Empty = (0,0)
cn (Node (_,_,_,Aprov x) e d) = (1+p+p2,x+q+q2)
    where (p,q) = cn e
          (p2,q2) = cn d
cn (Node x e d) = (p+p2,q+q2)
    where (p,q) = cn e
          (p2,q2) = cn d

mediaAprov2 :: Turma -> Float
mediaAprov2 turma = 100*fromIntegral b/fromIntegral a
    where (a,b) = cn turma

-- g
--cotador de passam e avaliados
cpc:: Turma -> (Int, Int)

cpc Empty = (0,0)
cpc (Node (_,_,_,Aprov x) e d) = (1+p+p2,1+q+q2)
    where (p,q) = cpc e
          (p2,q2) = cpc d
cpc (Node (_,_,_,Rep    ) e d) = (p +p2 ,1+q+q2)
    where (p,q) = cpc e
          (p2,q2) = cpc d
cpc (Node x e d) = (p+p2,q+q2)
    where (p,q) = cpc e
          (p2,q2) = cpc d

aprovAv :: Turma -> Float
aprovAv turma = fromIntegral a/fromIntegral b
    where (a,b) = cpc turma


