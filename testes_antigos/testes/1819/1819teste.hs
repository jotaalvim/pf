import Data.List
import Data.Char
import System.Random

myelemIndices :: Eq a => a-> [a] -> [Int]
myelemIndices x l = auxe 0 x l 

auxe :: Eq a => Int -> a -> [a] -> [Int]
auxe c x [] = []
auxe c x (h:t) = if x == h then c : (auxe (c+1) x t)
                 else (auxe (c+1) x t)


myisSubsequenceOf :: Eq a => [a] -> [a] -> Bool
myisSubsequenceOf [] l = True
myisSubsequenceOf l [] = False
myisSubsequenceOf (h:t) (h2:t2)
    | h == h2   = myisSubsequenceOf t t2
    | otherwise = myisSubsequenceOf (h:t) t2       


-- 2


data BTree a = Empty | Node a (BTree a) (BTree a)

mylookupAP :: Ord a => a -> BTree (a,b) -> Maybe b
mylookupAP x Empty = Nothing
mylookupAP x (Node (y,z) e d)
    | x == y = Just z
    | x <  y = mylookupAP x e
    | x >  y = mylookupAP x d
   

zipWithBT :: (a -> b -> c) -> BTree a -> BTree b -> BTree c
zipWithBT f Empty Empty = Empty
zipWithBT f (Node x e d) (Node x2 e2 d2) = Node (f x x2) (zipWithBT f e e2) (zipWithBT f d d2) 

-- 3 

digitAlpha :: String -> (String,String)
digitAlpha [] = ([],[])
digitAlpha (x:xs) | isDigit x = (x:a,b)
                  | isAlpha x = (a,x:b)
                  where (a,b) = digitAlpha xs

-- 4
{-
Seq a = Nil | Cons a (Seq a) | App (Seq a) (Seq a)



seq = App (Cons 4 Nil ) (App (Nil) (Cons 5 Nil) )

firstSeq :: Seq a -> a
firstSeq (Cons x s) = x 
firstSeq (App e d)
    | temNil e  = firstSeq d
    | otherwise = firstSeq e

temNil :: Seq a -> Bool
temNil (Cons x s ) = False
temNil (App e d) = temNil e && temNil d  
temNil Nil = True  





dropSeq :: Int -> Seq a -> Seq a
dropSeq n a = a'
    where (n',a') = daux n a 

daux :: Int -> Seq a -> (Int, Seq a) 
daux 0 s     = (0,  s)
daux n (Nil) = (n,Nil)
daux n (Cons x s) = daux (n-1) s

daux n (App e d)  = if ne == 0 
                    then (0,App se d)
                    
                    else daux (n-ne) d
    where (ne,se) = daux n e




instance (Show a) => Show (Seq a) where
    show (Nil) = ""
    show (Cons x s) = "<<" ++ show x ++ "," ++ show s ++ ">>"
    show (App e d ) =  
-}
-- 5    

type Mat a = [[a]]

mat :: Mat Int
mat = [[1,2,3],
       [5,6,7],
       [8,9,4]]
mat2 :: Mat Int
mat2 = [[6,7,2], [1,5,9], [8,3,4]]

getElem :: Mat a -> IO a
getElem l = do cs <- randomRIO (0,length (head l) -1)
               hs <- randomRIO (0,length l -1)
               return (l !! cs !! hs)




magic :: Mat Int -> Bool
magic [] = True
magic m  = all (==h) t
    where (h:t) = di (tran m) ++ di m ++ linhas m ++ linhas (tran m)

di :: Mat Int -> [Int]
di m = [sum [ m !! k !! k | k <- [0..length m -1]]]

tran :: Mat Int -> Mat Int
tran m = [ map (!!k) m | k <- [0..length m -1]]

linhas :: Mat Int -> [Int]
linhas m = map sum m 


