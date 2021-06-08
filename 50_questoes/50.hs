module Perguntas where


--import Data.List para testar as funcoes 
-- 1
myenumFromTo :: Int -> Int -> [Int]
myenumFromTo x y
    | x == y    = [x]
    | otherwise = x:myenumFromTo (x+1) y

myenumFromTo2 :: Int -> Int -> [Int]
myenumFromTo2 x y 
    | x > y = []
    | x == y    = [x]
    | otherwise = [x] ++ myenumFromTo2 (x+1) y

-- 2 


myenumFromThenTo :: Int -> Int -> Int -> [Int]
myenumFromThenTo x  y z
                 --10 7 1
    | x == z = [x]
    | x >= z && x == y = repeat x
    | x < z && x == y = []
    | (x > z && y >= z ) || (x < z && y <= z ) = x:myenumFromThenTo (y) (2*y-x) z 
    | otherwise = []



-- 3
mm :: [a]-> [a]-> [a]
mm [] l = l 
mm (a:b) l = a : mm b l


mm2 :: [a] -> [a] -> [a]-- [1,2,3] ++ [4,5,6] = [1,2,3,4,5,6]
mm2 x y
    | length x == 0 = y
    | otherwise = head x: mm2 (tail x) y
--duvida de (a:b na mm2)

-- 4
--existe e chama se !!
pl :: [a] -> Int -> a 
--pl [] x =. error " index to large"
pl (a:b) 0 = a
pl (a:b) x = pl b (x-1)


pl2 :: [a] -> Int -> a -- [1,2,3] 0 -> 1, [1,2,3] 2 -> 3
pl2 x y
    | length x == y+1 = last x 
    | otherwise       = pl2 (init x) y

-- 5
myreverse :: [a] -> [a] --[10,20,30] -> [30,20,10]
myreverse []= []
myreverse x = last x : myreverse(init x)

myreverse2 :: [a] -> [a]
myreverse2 [] = []
myreverse2 (x:y) = (myreverse2 y) ++ [x] 

myreverse3 :: [a] -> [a] 
myreverse3 l = aux [] l
aux l2 [] = l2
aux l2 (h:t) = aux (h:l2) t
-- r3 == r4 mas mais compacta 
myreverse4 :: [a] -> [a]
myreverse4 l = foldl (\a x -> x:a) [] l 



-- 6
mytake :: Int -> [a] -> [a]  --take 2 [10,20,30] -> [10,20].
mytake x l
    | x <= 0 = []
    | length l == x = l
    | otherwise     = mytake x (init l)

-- 7
mydrop :: Int -> [a] -> [a]-- 2 [10,20,30] -> [30].
mydrop 0 l = l
mydrop x [] = []
mydrop x (a:b) = mydrop (x-1) b

-- 8 
myzip :: [a] -> [b] -> [(a,b)]
myzip [] _ = []
myzip _ [] = []
myzip (a:b) (c:d) = (a,c): myzip b d

-- 9

myelem :: Eq a => a -> [a] -> Bool-- elem 3 [1,2,3,4,5]
myelem _ [] = False
myelem x l = if x == head l then True else myelem x (tail l)


-- 10 

myreplicate :: Int -> a -> [a]
myreplicate 0 y = [] 
myreplicate x y = y:myreplicate (x-1) y

myreplicate2 :: Int -> a -> [a]
myreplicate2 x y 
    | x == 0    = []
    | otherwise = y: myreplicate2 (x-1) y

-- 11

myintersperse :: a -> [a] -> [a] --1 [5,2,3] -> [5, 1, 2, 1, 3].
myintersperse _ [] = []
myintersperse _ [a] = [a]
myintersperse x (a:b) = a:x: myintersperse x b


-- 12

--[1,2,2,3] -> [[1],[2,2],[3]]
mygroup :: Eq a => [a] -> [[a]]
mygroup [] = []
mygroup [a] = [[a]]
mygroup (a:b)
    | a == head (c1) = (a:c1) : c2
    | otherwise = [a]: (c1:c2)
    where 
        (c1:c2) = mygroup b

 
--versão um pouco mais eficiente eficiente do que tinha antes
a :: Eq a => [a] -> [[a]]
a [x] = [[x]]
a l = x: a y
    where (x,y) = fa l 

fa :: Eq a => [a] -> ([a],[a])
fa []  = ([ ],[])
fa [a] = ([a],[])
fa (h:h2:t)
    | h == h2 = (h:p,q) 
    | otherwise = ([h],h2:t)
    where (p,q)  = fa (h2:t)

-- 13

myconcat :: [[a]] -> [a] 
myconcat [] = []--[[1],[2],[3,4]]
myconcat (a:b) = a ++ myconcat b 

-- 14

myinits :: [a] -> [[a]]--[1,2,3] -> [[], [1], [1,2], [1,2,3]]
myinits [] = [[]]
myinits l = myinits (init l) ++ [l]

-- 15

mytails :: [a] -> [[a]] 
mytails [] = [[]]
mytails l = l:(mytails (tail l)) 

--16

myisPrefixOf :: Eq a => [a]-> [a] -> Bool --[1,2,3] [1,2,3,5]-> True
myisPrefixOf [] _ = True
myisPrefixOf (a:b) (c:d)
--    | length (a:b) > length (c:d) = False
    | a == c = myisPrefixOf b d
    | otherwise = False

-- 17

myisSuffixOf :: Eq a => [a] -> [a] -> Bool --[2,3] [1,2,3] -> True
myisSuffixOf [] _ = True
myisSuffixOf l l2
    | length l > length l2 = False
    | last l ==  last l2 = myisSuffixOf (init l) (init l2)
    | otherwise = False

-- 18

myisSubsequenceOf :: Eq a => [a] -> [a] -> Bool --[2,4] [1,2,3,4] True
myisSubsequenceOf [] _ = True
myisSubsequenceOf _ [] = False
myisSubsequenceOf (a:b) (c:d)
    | a /= c = myisSubsequenceOf (a:b) d 
    | otherwise = myisSubsequenceOf b d

-- 19

--defenição não recursiva
--p19 :: Eq a => a -> [a] -> [Int]
--p19 x l = [ x | x <- l, x == ( head l)]



myelemIndices :: Eq a => a -> [a] -> [Int]
myelemIndices x l = elaux 0 x l

elaux :: Eq a => Int -> a -> [a] -> [Int]
elaux p x [] = []
elaux p x (h:t) | x == h = p: elaux (p+1) x t 
                | x /= h = elaux (p+1) x t

myelemIndices2 :: Eq a => a -> [a] -> [Int] -- 3 [1,2,3,4,3,2,3,4,5]-> [2,4,6]
myelemIndices2 _ [] = []
myelemIndices2 x l
    | last l == x = myelemIndices2 x (init l) ++ [(length l)-1]
    | otherwise   = myelemIndices2 x (init l)
-- 20

mynub :: Eq a => [a] -> [a] -- [1,2,1,2,3,1,2] -> [1,2,3]
mynub l = g [] l
g l2 [] = l2
g l2 (a:b) | elem a l2 = g l2 b 
           | otherwise = g (l2++[a]) b


v :: Eq a => [a] -> [a]
v [] = []
v (h:t) = if elem h t then h: v (el h t)
          else h: v t
          
-- elimina todo o x numa lista 
el :: Eq a => a-> [a]-> [a]
el x [] = []
el x (h:t)= if x == h then el x t
           else h:el x t


-- 21
mydelete :: Eq a => a -> [a] -> [a]
mydelete x [] = []
mydelete x (a:b) = if a == x then b else a:mydelete x b

mydelete2 :: Eq a => a -> [a] -> [a]
mydelete2 x l = gd [] x l
gd lc x [] = lc
gd lc x (a:b) = if a == x then lc ++ b else gd (lc ++ [a]) x b

-- 22

fb :: Eq a => [a] -> [a] -> [a]
fb l [] = l -- [1,2,3,4,5,1] [5,1] -> [2,3,4,1]
fb (a:b) (c:d) 
    | elem a (c:d) = fb b (reti a (c:d))
    | otherwise = a:fb b (c:d)
    where
        reti :: Eq a => a -> [a] -> [a]
        reti a (c:d) = if a == c then d else c:reti a d

-- 23

myunion :: Eq a => [a] -> [a] -> [a]
myunion l [] = l
myunion l (c:d) = if not (elem c l) then myunion (l ++ [c]) d else myunion l d 

-- 24

myintersect :: Eq a => [a] -> [a] -> [a]  --[1,1,2,3,4] [1,3,5]->[1,1,3]
myintersect l []  = []
myintersect [] l = []
myintersect (a:b) l = if elem a l then a:myintersect b l else myintersect b l

-- 25 

myinsert :: Ord a => a -> [a] -> [a]
myinsert x [] = [x]
myinsert x (a:b) = if x <= a then x:(a:b) else a:myinsert x b   

--26
myunwords :: [String]-> String
myunwords [] = []
myunwords [a] = a
myunwords (h:t) = h ++ " " ++ myunwords t 
 
--27

myunlines :: [String] -> String
myunlines [] = []
myunlines ([]:c) = '\n':myunlines c
myunlines ((a:b):c) = a:myunlines (b:c)

p271 :: [String] -> String
-- ["ola", "57"] ="ola\n57"
p271 []    = "" -- []
--p271 [a]   = a ++ ['\n']
p271 (h:t) = h ++ ['\n'] ++ p271 t 
 
-- 28 

pMaior :: (Ord a) => [a] -> Int
pMaior (a:b) = gg (a,0) b

gg (v,i) [] = i
gg (v,i) l
    | last l > v = gg ( last l ,length l) il
    | otherwise = gg (v,i) il
    where il = init l


sel :: [a] -> Int -> a --existe e chama se !!
sel [] x = error " index to large"
sel (a:b) x = if x == 0 then a
              else sel b (x-1)
--sel [1,2,3,4] 0 == 1
-- pmaior [1,2,8,4] == 2
pmaior ::(Ord a ) => [a] -> Int
pmaior [] = error "nao esta defenido"
pmaior [a] = 0
pmaior (a:b) = if a > (sel b p) then 0
               else p + 1 
    where p = pmaior b

-- 29

temRepetidos :: Eq a => [a] -> Bool
temRepetidos [] = False
temRepetidos (a:b) = if elem a b then True else temRepetidos b

-- 30

algarismos :: [Char] -> [Char]
algarismos [] = []
algarismos (a:b) = if elem a ['0'..'9'] then a:algarismos b 
                   else algarismos b 

-- 31

posImpares :: [a] -> [a]
posImpares (_:x1:xs) = x1 : posImpares xs
posImpares [a] = []
posImpares [] = []

--posImpares2 :: [a] -> [a]
--posImpares2 l = [fst x | x <-(zip l $ cycle [1,0]), snd x == 1]

posImpares3 :: [a] -> [a]
posImpares3 l = [x | (x,y) <-(zip l $ cycle [0,1]), y == 1]

-- 32

posPares :: [a] -> [a]
posPares (h0:_:t) = h0:posPares t
posPares [x] = [x]
posPares []  =  []

posPares2 :: [a] -> [a]
posPares2 l = [x | (x,y) <-(zip l $ cycle [0,1]), y == 0]

posPares3 :: [a] -> [a]
posPares3 (a:b) = a:posPares3 (drop 1 b)
posPares3 [] = []

-- 33

isSorted :: Ord a => [a] -> Bool
isSorted (a:b:c) = if a <= b then isSorted (b:c)else False
isSorted [a] = True
isSorted [] = True

-- 34

iSort :: Ord a => [a] -> [a]
iSort [] = [] 
iSort (h:t) = insere h (iSort t)

insere :: Ord a => a -> [a] -> [a]
insere x [] = [x]
insere x (h:t) = if x <= h then (x:h:t) 
                 else h:insere x t 
--outros métodos de ordenar

minSort :: Ord a => [a] -> [a]
minSort [] = []
minSort l = m:(minSort t)
    where 
        m = minimo l
        t = retira m l
minimo :: Ord a => [a]-> a
minimo [a] = a
minimo (h:t) = min h (minimo t)
retira :: Ord a => a -> [a] -> [a]
retira x [] = [] 
retira x (h:t) = if h == x then t 
                 else h:retira x t

quickSort :: Ord a => [a] -> [a]
quickSort [] = []
quickSort (h:t) = (quickSort meno)++(h:(quickSort maio))
    where (meno, maio) = menMai h t 
          menMai :: Ord a => a -> [a] -> ([a],[a])
          menMai x [] = ([],[])
          menMai x (a:as) = if x < a then  (p,a:q) else (a:p,q)
              where (p,q) = menMai x as 

mergeSort :: Ord a => [a] ->[a]
mergeSort [] = [] 
mergeSort [a] = [a]
mergeSort l = merge (mergeSort l1) (mergeSort l2)

    where 
        (l1,l2)= parte l
        merge :: Ord a => [a] -> [a] -> [a]
        merge x [] = x
        merge [] y = y  
        merge (x:xs) (y:ys) = if x > y then y:merge (x:xs) (ys)
                              else x: merge xs (y:ys)
parte :: [a] -> ([a],[a])
parte [a] = ([a],[])
parte []  = ([],[])
parte (h:h2:t)  = (h:p,h2:q)
    where (p,q) = parte t

-- 35

menor :: String -> String -> Bool
menor (x:xs) "" = False
menor "" (x:xs) = True
menor (x:xs) (a:as)
    | x < a = True
    | x > a = False
    | x == a = menor xs as

-- 36

elemMSet :: Eq a => a -> [(a,Int)] -> Bool
elemMSet _ [] = False
elemMSet x (h:t) = x == fst h || elemMSet x t

-- 37

lengthMSet :: [(a,Int)] -> Int
lengthMSet [] = 0
lengthMSet (h:t) = snd h + ( lengthMSet t)

-- 38

converteMSet :: [(a,Int)] -> [a]
converteMSet [(a,1)] = [a]
converteMSet ((a,b):t) = if b /= 0 then a:converteMSet( (a,b-1):t ) else converteMSet t

-- 39

insereMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
insereMSet a (h:t) = if a == fst h then ((fst h,snd h +1):t) else h:insereMSet a t
insereMSet a [] = [(a,1)]


-- 40

removeMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
removeMSet x [] = []
removeMSet x ((a,b):t)
    | x == a = if b-1 == 0 then t else ((a,b-1):t)
    | otherwise = (a,b): removeMSet x t

-- 41
{-
constroiMSet :: Ord a => [a] -> [(a,Int)]
constroiMSet [] = []
constroiMSet (l:ls) = insereMSet l (constroiMSet ls)

-}

constroiMSet :: Ord a => [a] -> [(a,Int)]
constroiMSet []    = []
constroiMSet (h:t) = (h,c):constroiMSet (drop c (h:t))
    where c = constroiMSetAux (h:t)

constroiMSetAux ::Eq a => [a] -> Int 
constroiMSetAux []       = 0
constroiMSetAux [a]      = 1
constroiMSetAux (h:ht:t) = if h == ht then 1 + constroiMSetAux (ht:t) else 1

-- 42

p42 :: [Either a b] -> ([a],[b])
p42 [] = ([],[])

p42 (Left x:t)  = (x:p , q)
    where (p,q) = p42 t 

p42 (Right x:t) = (p, x:q )
    where (p,q) = p42 t 


-- 43

catMaybes' :: [Maybe a] -> [a]
catMaybes' [] = []
catMaybes' (Just a:t)  = a:catMaybes' t
catMaybes' (Nothing:t) = catMaybes' t

-- 44

data Movimento = Norte | Sul | Este | Oeste
    deriving Show

posicao :: (Int,Int) -> [Movimento] -> (Int,Int)
posicao (x,y) []    = (x,y)
posicao (x,y) (h:t) = posicao (posicaox (x,y) h) t

posicaox (x,y) Norte = (x+1,y)
posicaox (x,y) Sul   = (x-1,y)
posicaox (x,y) Este  = (x,y+1)
posicaox (x,y) Oeste = (x,y-1)

-- 45

caminho :: (Int,Int) -> (Int,Int) -> [Movimento]
caminho (x,y) (x2,y2)
    | x < x2 = Este : caminho (x+1,y) (x2,y2)
    | x > x2 = Oeste: caminho (x-1,y) (x2,y2)
    | y < y2 = Norte: caminho (x,y+1) (x2,y2)
    | y > y2 = Sul  : caminho (x,y-1) (x2,y2)
    | otherwise = []

-- 46

vertical :: [Movimento] -> Bool
vertical [] = True

--vertical (x:xs) = case x of Este -> False
--                            Oeste-> False
--                            _ -> vertical xs
vertical (Este:t) = False
vertical (Oeste:t) = False
vertical (x:t) = vertical t

-- 47
data Posicao = Pos Int Int
    deriving Show

maisCentral :: [Posicao] -> Posicao
maisCentral [a] = a
maisCentral (h:t) = if dp h < dp (mc) then h else mc
    where mc = maisCentral t

--dp calcula a distancia
dp :: Posicao -> Float
dp (Pos a b) = sqrt( fromIntegral (a^ 2 + b^2) ) 

-- 48

vizinhos :: Posicao -> [Posicao] -> [Posicao]
vizinhos x [] = []
vizinhos (Pos x y) ((Pos x2 y2):ys) = if abs (x - x2) == 1 && y == y2 || abs (y - y2) == 1 && x == x2
                                      then Pos x2 y2 : vizinhos (Pos x y) ys 
                                      else vizinhos (Pos x y) ys

-- 49 

mesmaOrdenada :: [Posicao] -> Bool
mesmaOrdenada [] = True
mesmaOrdenada [a] = True 
mesmaOrdenada ((Pos x y ):(Pos x2 y2): xs) = y == y2 && mesmaOrdenada ((Pos x2 y2): xs)


-- 50

data Semaforo = Verde | Amarelo | Vermelho
    deriving Show

interseccaoOK :: [Semaforo] -> Bool
interseccaoOK [] = True
interseccaoOK l = (contador l) <= 1

-- mais que 1 é False

contador :: [Semaforo] -> Int
contador [] = 0
contador (Verde:t)  = 1 + contador t
contador (Amarelo:t)= 1 + contador t
contador (h:t) = contador t




