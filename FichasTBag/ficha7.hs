module Ficha7 where

data BTree a = Empty
             | Node a (BTree a) (BTree a) deriving Show

data LTree a = Tip a | Fork (LTree a) (LTree a) deriving Show

contanodos :: BTree a -> Int
contanodos Empty = 0
contanodos (Node x e d) = 1 + (contanodos e) + (contanodos d)

--Exercicio 1

data ExpInt = Const Int
 | Simetrico ExpInt
 | Mais ExpInt ExpInt
 | Menos ExpInt ExpInt
 | Mult ExpInt ExpInt

e = (Mult (Mais (Simetrico (Const (-2))) (Const 3)) (Menos (Const 5) (Simetrico (Mult (Const (-1)) (Const 3)))))


calcula :: ExpInt -> Int
calcula (Const x)   = x
calcula (Simetrico x) = (-1) * calcula x
calcula (Mais x y)  = (calcula x) + (calcula y)
calcula (Menos x y) = (calcula x) - (calcula y)
calcula (Mult x y)  = (calcula x) * (calcula y)

infixa :: ExpInt -> String 
infixa (Const x) = show x
infixa (Simetrico x) = "(-(" ++ infixa x ++ ")"
infixa (Mais x y) = "(" ++ infixa x ++ " + " ++ infixa y ++ ")"
infixa (Menos x y) = "(" ++ infixa x ++ " - " ++ infixa y ++ ")"
infixa (Mult x y) = "(" ++ infixa x ++ " * " ++ infixa y ++ ")"


posfixa :: ExpInt -> String
posfixa (Const x) = " " ++ show x
posfixa (Simetrico x) = posfixa x ++ " *(-1)"
posfixa (Mais x y) = posfixa x ++ posfixa y ++ " +"
posfixa (Menos x y) = posfixa x ++ posfixa y ++ " -"
posfixa (Mult x y) = posfixa x ++ posfixa y ++ " *"


----------------------------------------------------------------------------------------------------------------------------------------------------------

t = Fork (Fork (Tip 5) (Tip 7)) (Fork (Tip 1) (Fork (Tip 3) (Tip 2)))

ltSum :: Num a => LTree a -> a
ltSum (Tip x) = x
ltSum (Fork e d) = ltSum e + ltSum d

listLT :: LTree a -> [a]
listLT (Tip x) = [x]
listLT (Fork e d) = listLT e ++ listLT d

ltHeight :: LTree a -> Int
ltHeight (Tip x) = 1
ltHeight (Fork e d) = 1 + (max (ltHeight e) (ltHeight d))

mapLT :: (a -> b) -> LTree a -> LTree b
mapLT f (Tip x) = Tip (f x)
mapLT f (Fork e d) = (Fork (mapLT f e) (mapLT f d))

-------------
data FTree a b = Leaf b | No a (FTree a b) (FTree a b) deriving Show

t2 = No 5 (No 4 (Leaf 'C') (Leaf 'b')) 
          (No 1 (No 6 (Leaf 'S') (Leaf '5')) (Leaf 'h'))

splitFTree :: FTree a b -> (BTree a, LTree b)
splitFTree (Leaf x) = (Empty, Tip x)
splitFTree (No x e d) = (Node x (b1) (b2), Fork (l1) (l2)) 
                  where (b1,l1) = splitFTree e
                        (b2,l2) = splitFTree d

joinTrees :: BTree a -> LTree b -> Maybe (FTree a b)
joinTrees Empty (Tip x) = Just (Leaf x)
joinTrees _ (Tip x) = Nothing
joinTrees Empty (Fork e d) = Nothing
joinTrees (Node y e1 d1) (Fork e d) = 
    let lt1 = joinTrees e1 e
        lt2 = joinTrees d1 d
    in case lt1 of
        Nothing   -> Nothing
        (Just t1) -> case lt2 of 
                      Nothing   -> Nothing
                      (Just t2) -> Just (No y t1 t2)

data RTree a = R a [RTree a] deriving Show

t1 = R 5 [R 3 [], R 4 [R 2 [], R 1 [], R 7 []]]

soma :: Num a => RTree a -> a
soma (R x lrt) = x + sum (map (soma) lrt)

altura :: RTree a -> Int
ãltura (R x [])  = 1
altura (R x lrt) = 1 + maximum (map (altura) lrt)

prune :: Int -> RTree a -> RTree a
prune 0 (R x _)  = R x []
prune _ (R x []) = (R x [])
prune n (R x lrt) = R x (map (prune (n-1)) lrt)

mirror :: RTree a -> RTree a
mirror (R x lrt) = R x (reverse (map (mirror) lrt))

postorder :: RTree a -> [a]
postorder (R x lrt) = (concat (map (postorder) lrt)) ++ [x]

