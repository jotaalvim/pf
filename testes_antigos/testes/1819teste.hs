import System.Random



-- 2
{-
data BTree a = Empty | Node a (BTree a) (BTree a)

mylookupAP :: Ord a => a -> BTree (a,b) -> Maybe b
mylookupAP x Empty = Nothing
mylookupAP x (Node (y,z) e d)
	| z == y = Just z
	| z < y = mylookupAP x e
	| z > y = mylookupAP x d


zipWithBT :: (a -> b -> c) -> BTree a -> BTree b -> BTree c
zipWithBT f Empty Empty = Empty
zipWithBT f (Node x e d) (Node x2 e2 d2) = Node (f x x2) (zipWithBt e e2) (zipWithBt d d2) 
-}













-- 5
type Mat a = [[a]]

mat = [[1,2,3],
       [5,6,7],
       [8,9,4]]
mat2 = [[6,7,2], [1,5,9], [8,3,4]]

getElem :: Mat a -> IO a
getElem l = do cs <- randomRIO (0,length (head l) -1)
               hs <- randomRIO (0,length l -1)
               return (l !! cs !! hs)


-- th :: Mat a -> Bool

th l = all (== head k) k
    where k = map sum l