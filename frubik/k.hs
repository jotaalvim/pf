data RTree a = R a [RTree a] deriving (Show)

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

-- [Just [1],Just [2],Just [3]  ]

-- [1,2,3,4]

catM :: [Maybe [a]]-> [a]
catM [] = []
catM (Nothing:t) = catM t
catM ((Just x):t) = x ++ catM t
