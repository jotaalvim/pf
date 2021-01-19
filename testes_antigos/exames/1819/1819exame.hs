import Data.List 


t = [Just 3, Just 5, Just 89, Nothing, Just 100] 

maximumMB :: (Ord a) => [Maybe a] -> Maybe a
maximumMB l = Just $ maximum (aux l) 
    where aux [] = []
          aux ((Just x):t) = x : aux t
          aux (Nothing:t)  =     aux t 










type RelP a = [(a , a)]
type RelL a = [( a , [a] )]
type RelF a = ([a] ,a -> [a] )

relp :: RelP Int
relp = [(1,3),(1,4),(2,1),(2,4),(2,5),(3,7),(4,7),(5,7),(6,5),(7,6)] 
rell :: RelL Int
rell = [(1,[3,4]),(2,[1,4,5]),(3,[7]),(4,[7]),(5,[7]),(6,[5]),(7,[6])] 

relf :: RelF Int
relf = ([1,2,3,4,5,6,7],f)
    where f 1 = [3,4]
          f 2 = [1,4,5] 
          f 3 = [7]
          f 4 = [7]
          f 5 = [7]
          f 6 = [5]
          f 7 = [6]

convLP :: RelL a -> RelP a
convLP l = concat (map junta l)
    where junta (x,xs) = map (\y->(x,y)) xs

{-
convPL :: (Eq a) => RelP a -> RelL a
convPL [] = []
convPl l@((x,y):t) = (x,y:fil): convPl s
    where (fil,s) = filtra x t

filtra ::(Eq a) => a -> Ralp a -> ([a], Ralp a)
filtra x [] = ([],[])
filtra x ((y,z):t) = if x == y then (z:p,q) else (p,(y,z):q)
    where (p,q) = filtra x t
-}

criaRelPint :: Int -> IO (RelP Int)
criaRelPint 0 = return []
criaRelPint n = do putStr " insira um par de interios: "
                   par <- getLine-- pode ser preciao usar um read"(2,3)" :: (Int,Int)
                   l <- criaRelPint (n-1)
                   return (read par:l)

{-

convFP :: (Eq a) => RelF a -> RelP a
convFP (l,f) = [ (x,z) | x <- l,z <- (f x) ]

--convFP (l,g) = convLP map(\x -> (x, g x)) l

--FIXME
convPF ::  (Eq a) => RelP a -> RelF a
convPF l = (nub ((map fst l)++(map snd l)) , fun l)
    where fun :: (Eq a) => RelP a -> a -> [a]
          fun l x = map snd (filter (\(y,_)-> y == x) l)

-}







