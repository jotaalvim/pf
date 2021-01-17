data BTree a = Empty
             | Node a (BTree a) (BTree a) deriving Show

arv :: BTree Int
arv = (Node 10 (Node 5 (Node 2 (Node 1 Empty Empty) (Node 4 (Node 3 Empty Empty) Empty)) (Node 7 (Node 6 Empty Empty) (Node 8 Empty (Node 9 Empty Empty))) )
               (Node 15 (Node 12 (Node 11 Empty Empty) (Node 14 (Node 13 Empty Empty) Empty)) (Node 17 (Node 16 Empty Empty) (Node 18 Empty (Node 19 Empty Empty)))))

arv1 = (Node 10 (Node 5 (Node 2 Empty (Node 4 (Node 3 Empty Empty) Empty)) Empty )
               (Node 15 (Node 12 (Node 11 Empty Empty) (Node 14 Empty Empty)) Empty))


altura :: BTree a -> Int
altura Empty = 0
altura (Node x e d) = 1 + (max (altura e) (altura d))

contanodos :: BTree a -> Int
contanodos Empty = 0
contanodos (Node x e d) = 1 + (contanodos e) + (contanodos d)

folhas :: BTree a -> Int
folhas Empty = 0
folhas (Node _ Empty Empty) = 1
folhas (Node _ e d) = folhas e + folhas d

prune :: Int -> BTree a -> BTree a
prune 0 _ = Empty
prune _ Empty = Empty
prune n (Node x e d) = (Node x (prune (n-1) e) (prune (n-1) d))

path :: [Bool] -> BTree a -> [a]
path _ Empty = []
path [] (Node x _ _) = [x]
path (y:ys) (Node x e d)
 | y = x : path ys d
 | otherwise = x : path ys e

mirror :: BTree a -> BTree a
mirror Empty = Empty
mirror (Node x e d) = (Node x (mirror d) (mirror e))

zipWithBT :: (a->b->c) -> BTree a -> BTree b -> BTree c
zipWithBT f (Node x e1 d1) (Node y e2 d2) = Node (f x y) (zipWithBT f e1 e2) (zipWithBT f d1 d2)
zipWithBT _ _ _ = Empty

unzipBT :: BTree (a,b,c) -> (BTree a, BTree b, BTree c)
unzipBT Empty = (Empty, Empty, Empty)
unzipBT (Node (x,y,z) e d) = (Node x a1 a2, Node y b1 b2, Node z c1 c2)
      where (a1,b1,c1) = unzipBT e
            (a2,b2,c2) = unzipBT d

minimo :: Ord a => BTree a -> a
minimo (Node x Empty _) = x
minimo (Node x e d) = minimo e

semMinimo :: Ord a => BTree a -> BTree a
semMinimo (Node x Empty d) = d
semMinimo (Node x e d) = Node x (semMinimo e) d

minSmin :: Ord a => BTree a -> (a,BTree a)
minSmin (Node x Empty d) = (x,d)
minSmin (Node x e d) = (m, Node x sm d)
     where (m,sm) = minSmin e

remove :: Ord a => a -> BTree a -> BTree a
remove x Empty = Empty
remove x (Node y e d)
 | y == x = let (m,smd) = minSmin d
            in (Node m e smd)
 | y < x = Node y (remove x e) d
 | x > y = Node y e (remove x d)

------
type Aluno = (Numero,Nome,Regime,Classificacao)
type Numero = Int
type Nome = String
data Regime = ORD | TE | MEL deriving Show
data Classificacao = Aprov Int
 | Rep
 | Faltou
 deriving Show
type Turma = BTree Aluno -- ´arvore bin´aria de procura (ordenada por n´umero)

a0 = (0,"Arlinda",ORD, Aprov 12)
a1 = (1,"Asdrubal",TE, Aprov 10)
a2 = (2,"Felismino", MEL, Rep)
a3 = (3,"Chinoca Inteligente",ORD, Aprov 20)
a4 = (4,"Chinoca Burro",ORD, Faltou)

t = (Node a2 (Node a1 (Node a0 Empty Empty) Empty) (Node a3 Empty (Node a4 Empty Empty))) :: Turma  

inscNum :: Numero -> Turma -> Bool
inscNum n Empty = False
inscNum n (Node (x,_,_,_) e d)
 | n == x = True
 | n < x  = inscNum n e
 | otherwise = inscNum n d 

inscNome :: Nome -> Turma -> Bool
inscNome n Empty = False
inscNome n (Node (_,x,_,_) e d)
 | n == x = True
 | otherwise = inscNome n e || inscNome n d

trabEst :: Turma -> [(Numero,Nome)]
trabEst Empty = []
trabEst (Node (n,no,TE,_) e d) = (trabEst e) ++ [(n,no)] ++ (trabEst d) 
trabEst (Node _ e d) = (trabEst e) ++ (trabEst d) 

nota :: Numero -> Turma -> Maybe Classificacao
nota n Empty = Nothing
nota x (Node (n,_,_,c) e d)
 | x == n = Just c
 | x <  n = nota x e
 | otherwise = nota x d

percFaltas :: Turma -> Float
percFaltas t = 100* contaFaltas t / (fromIntegral (contanodos t))
         where contaFaltas :: Turma -> Float
               contaFaltas Empty = 0
               contaFaltas (Node (_,_,_,Faltou) e d) = 1 + contaFaltas e + contaFaltas d
               contaFaltas (Node _ e d) = contaFaltas e + contaFaltas d

mediaAprov :: Turma -> Float
mediaAprov t = divisao (nota,na)
         where divisao :: (Float, Float) -> Float
               divisao (a,b) = a/b
               notas :: Turma -> (Float, Float)
               notas Empty = (0,0)
               notas (Node (_,_,_,Aprov x) e d) = ((fromIntegral x)+(fst (notas e))+(fst (notas d)), 1 + (snd (notas e)) + (snd (notas d)))
               notas (Node _ e d) = ((fst (notas e))+(fst (notas d)),(snd (notas e)) + (snd (notas d)))
               (nota,na) = notas t


aprovAv :: Turma -> Float
aprovAv t = 100 * np/na
      where contaAv :: Turma -> (Float,Float)
            contaAv Empty = (0,0)
            contaAv (Node (_,_,_,Aprov x) e d) = (1+(fst(contaAv e)) + (fst(contaAv d)),1+(snd(contaAv e)) + (snd(contaAv d)))
            contaAv (Node (_,_,_,Rep) e d) = ((fst(contaAv e)) + (fst(contaAv d)),1+(snd(contaAv e)) + (snd(contaAv d)))
            contaAv (Node _ e d) =  ((fst(contaAv e)) + (fst(contaAv d)),(snd(contaAv e)) + (snd(contaAv d)))
            (np,na) = contaAv t