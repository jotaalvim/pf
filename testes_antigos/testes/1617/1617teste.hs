import Data.List
-- 1

type MSet a = [(a,Int)]
ex :: MSet Char
ex =  [('b',4),('a',2),('c',1)]

cardMSet :: MSet a -> Int
cardMSet l = foldl (\a (x,y) -> a +y) 0 l
--           sum $ map snd l
-- b

moda :: MSet a -> [a]
moda [] = []
moda l = replicate b a
    where (a,b) = last $ sortOn snd l

-- converteMSet [('b',4),('a',2),('c',1)] == "bbbbaac"
converteMSet :: MSet a -> [a]
converteMSet li = foldl (\a (x,y) -> a ++ replicate y x) [] li



-- addNcopies [('b',4),('a',2),('c',1)] 'x' 5
addNcopies :: Eq a => MSet a -> a -> Int -> MSet a
addNcopies [] c n = [(c,n)]
addNcopies l@((c1,n1):t) c2 n2 
    | n2 >= n1 = (c2,n2):l
    | otherwise = (c1,n1): addNcopies t c2 n2

-- 2 
data SReais = AA Double Double 
            | FF Double Double
            | AF Double Double 
            | FA Double Double
            | Uniao SReais SReais

t = Uniao (Uniao (AA 4.2 5.5) (AF 3.1 7.0)) (FF (-12.3) 30.0)

instance Show (SReais) where
    show (Uniao e d) = "(" ++ show e++" U "++ show d++")"
    show (AA n1 n2)  = "(]"++ show n1 ++","++ show n2++"[)" 
    show (FF n1 n2)  = "(["++ show n1 ++","++ show n2++"])"
    show (AF n1 n2)  = "(]"++ show n1 ++","++ show n2++"])"
    show (FA n1 n2)  = "(["++ show n1 ++","++ show n2++"[])"

pertence :: Double-> SReais -> Bool
pertence n (Uniao e d) = pertence n e || pertence n d
pertence n (AA n1 n2) =  n1 <  n && n > n2
pertence n (FF n1 n2) =  n1 <= n && n >= n2
pertence n (AF n1 n2) =  n1 <  n && n >= n2
pertence n (FA n1 n2) =  n1 <= n && n > n2


{-
tira :: Double -> SReais -> SReais
tira n (Uniao n1 n2) = Uniao (tira n n1) (tira n n2)
tira n (AA n1 n2) = if pertence n  Uniao (AA n1 n) (AA n n2)
-}

catM :: [Maybe [a]]-> [a]
catM [] = []
catM (Nothing:t) = catM t
catM ((Just x):t) = x ++ catM t

{-
data RTree a = R a [RTree a]
percorre :: [Int] -> RTree a -> Maybe [a]

percorre [] _ = Just []
percorre (h:t) (R a l) = a: percorre t (l !! h)




-- b
ex = R 4 [ R 5 [],
           R 6 [R 9[]]]


procura :: Eq a => a -> RTree a -> Maybe [a]
procura x (R z []) = if x == z
                     then Just [x]
                     else Nothing

procura x (R z l)  = if x == z
                     then Just [x]
                     else if null $ catM ac
                          then Nothing
                          else Just ( z: catM ac)
    where ac = [ procura x filho| filho <- l ]

-}


{-

procura2 :: Eq a => a -> RTree a -> Maybe [Int]
procura2 x (R z []) = if x == z then 


--pa:: => Eq a => a -> RTree a ->([Int],Int,RTree a)
pa x (R z [])
    | x == z = ([1],1, R z []) 
    | x /= z = ([] ,0, R z [])

pa x (R z l)
    | x == z = 
    | x /= z = 
-}



