data ExpInt =Const Int
           | Simetrico ExpInt
           | Mais ExpInt ExpInt
           | Menos ExpInt ExpInt
           | Mult ExpInt ExpInt
    deriving (Show)




ex1:: ExpInt
ex1 = Mais (Const 3) (Mult (Const 7) (Const 5))

calcula :: ExpInt -> Int
calcula (Const x) = x
calcula (Simetrico e) = -(calcula e)
calcula (Mais e1 e2)  = (calcula e1)+ (calcula e2)
calcula (Menos e1 e2) = (calcula e1) - (calcula e2)
calcula (Mult e1 e2)  = (calcula e1) * (calcula e2) 


infixa :: ExpInt -> String
infixa (Const x) = show x
infixa ( Simetrico e ) = "- ( "++((infixa e))++" )"
infixa (Mais e1 e2)    = '(':' ':(infixa e1)++" + "++ (infixa e2)++" )"
infixa (Menos e1 e2)   = '(':' ':(infixa e1)++" - "++ (infixa e2)++" )"
infixa (Mult e1 e2)    = '(':' ':(infixa e1)++" * "++ (infixa e2)++" )"


posfixa :: ExpInt -> String
posfixa (Const x) = show x
posfixa (Simetrico e) = (posfixa e)++" ~"
posfixa (Mais e1 e2 ) = (posfixa e1)++' ':(posfixa e2) ++ " +"
posfixa (Menos e1 e2 ) = (posfixa e1)++' ':(posfixa e2) ++ " -"
posfixa (Mult e1 e2 ) = (posfixa e1)++' ':(posfixa e2) ++ " *"
-- tentar calulaar um pos fixa inver disto " 3 7 5 * + 10 20 + -"


-- 2

arv = R 5 [ R 4 [ R 3 [R 17 []], 
                  R 2 [], 
                  R 7 []],
            R 10 [], 
            R 1 [ R 8 [R 0 [],  
                       R 20 [],  
                       R 15 []], 
           
            R 12 [] ]]


data RTree a = R a [RTree a] deriving (Show)

soma :: Num a => RTree a -> a
soma (R x l) = x + sum (map soma l) 

altura :: RTree a -> Int 
altura (R x []) = 1 
altura (R x l)  = 1 + maximum (map altura l)

prune :: Int -> RTree a -> RTree a
prune n (R x l) | n == 1 = (R x l)
                | n > 1 = (R x (map (prune (n-1)) l ))

mirror :: RTree a -> RTree a
mirror (R x [] ) = R x []
mirror (R x l ) = R x (reverse (map mirror l ))

postorder :: RTree a -> [a]
postorder (R x []) = [x]
postorder (R x l)  = concat(map postorder l) ++ [x]

paths :: RTree a -> [[a]]
paths (R x []) = [[x]]
paths (R x l ) = map (x:) (concat (map paths l ))

paths2 :: RTree a -> [[a]]
paths2 (R x []) = [[x]]
paths2 (R x l)  =     [ x : f    | f <- lf]
    where lf = concat [ paths2 f | f <- l ]
-- 3



data BTree a = Empty | Node a (BTree a) (BTree a)
    deriving(Show)
data LTree a = Tip a | Fork (LTree a) (LTree a)
    deriving(Show)



larv = Fork (Fork (Tip 7) (Tip 8) ) (Fork (Tip 4) (Fork (Tip 1) (Tip 6)))

lfSum :: Num a => LTree a -> a
lfSum (Tip x) = x
lfSum (Fork e d) = lfSum e + lfSum d

listaLT :: LTree a -> [a]
listaLT (Tip x )= [x]
listaLT(Fork e d ) = (listaLT e) ++ (listaLT d)

ltHeight :: LTree a -> Int
ltHeight (Tip x) = 0
ltHeight (Fork e d) = 1+ max (ltHeight e) (ltHeight d)

-- 4 



data FTree a b = Leaf b | No a (FTree a b) (FTree a b)
    deriving(Show)


farv = No 1 (No 4 (No 5 (Leaf 'a') (Leaf 'b')) (Leaf 'c'))
            (No 7 (Leaf 'd') (Leaf 'e'))



--data LTree a = Tip a | Fork (LTree a) (LTree a)

splitFTree :: FTree a b -> (BTree a, LTree b)
splitFTree (Leaf x)   = (Empty, Tip x)
splitFTree (No x e d2) = (Node x a1 a2,Fork b1 b2)
    where (a1,b1) = splitFTree e 
          (a2,b2) = splitFTree d2
          

--data BTree a = Empty  | Node a (BTree a) (BTree a)
--data LTree a =  Tip a | Fork (LTree a) (LTree a)
--data FTree a b = Leaf b | No a (FTree a b) (FTree a b)

joinTrees :: BTree a -> LTree b -> Maybe (FTree a b)
joinTrees Empty (Tip a)        = Just (Leaf a)
joinTrees Empty (Fork e d)     = Nothing
joinTrees (Node x e d) (Tip a) = Nothing

joinTrees (Node x e d) (Fork e' d') = aux (ef,df)

    where ef = (joinTrees e e')
          df = (joinTrees d d')
          
          aux (Just a, Just b) = Just (No x a b)
          aux _ = Nothing







