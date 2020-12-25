
sel :: [a] -> Int -> a --existe e chama se !!
sel [] x = error " index to large"
sel (a:b) x = if x == 0 then a
              else sel b (x-1)


--sel [1,2,3,4] 0 == 1

pmaior :: Ord a => [a] -> Int
pmaior [] = error "nao esta defenido"
pmaior [a] = 0
pmaior (a:b) = if a > (sel b p) then 0
               else p + 1
    where p = pmaior b

{-
pmaior [1,2,3]
a = 1, b = [2,3]

    p = pmaior [2,3]
    a = 2 , b = [3]

        p = pmaior [3]
        p = 0
    
    2 > 3-- sel [3] 0
    p = 1
1 > 3 
p = 2

2

-}



--pmaior2 = snd (pmaiorAux l)

pmaiorAux :: Ord a => [a] -> (a,Int)
pmaiorAux [x] = (x,0)
pmaiorAux (h:t) = if h > m then (h,0) else (m,p+1) 
    where (m,p) = pmaiorAux t

-- ordena uma lista



-- insertion sort
iSort :: Ord a => [a] -> [a]
iSort [] = []
iSort (h:t) = insere h t'
    where t' = iSort t--vamos imaginar que temos um gajo meu amigo que sabe ordenar listas mais pequeninas

insere :: Ord a => a -> [a] -> [a]
-- insere x e assume que l esta ordenada
-- retoma lista ordenada
insere x [] = [x]
insere x (h:t) = if x<=h then (x:h:t) 
                 else h:insere x t

--minimun sort
minSort :: Ord a => [a] -> [a]
minSort [] = []
minSort l = h:(minSort t)
    where h = minimo l-- h é o menor elemento de l
          t = remove h l  -- t é a lista removendo o h

minimo :: Ord a => [a] -> a
minimo [] = error "Empty list"
minimo [a] = a
minimo (h:t) = min h (minimo t)

remove ::Eq a => a -> [a] -> [a]
remove x  [y]   = []
remove x  (h:t) = if x == h then t 
                  else x:remove x t 


-- l = [7,2,5,3,8,9]
-- h = 7, men = [2,5,3], mai = [8,9]

quickSort :: Ord a => [a]-> [a]
quickSort [] = []
quickSort (h:t) = (quickSort men) ++ (h: (quickSort mai)) 
    where (men,mai) = menMai h t  
          --men = [x | x <- t, x <= h]--menores h t --lista dos elementos de t que são <= h 
          --mai = [x | x <- t, x > h]--maiores h t --lista dos elementos de t que são < h 
          --menores :: Ord a => a -> [a] -> [a]
          --menores x [] = []
          --menores x (h:t) | x < h     = x:menores x t
          --                | otherwise = menores x t
          --maiores :: Ord a => a -> [a] -> [a]
          --maiores x [] = []
          --maiores x (h:t) | x > h     = x:maiores x t
          --                | otherwise = maiores x t
          menMai :: Ord a => a -> [a] -> ([a],[a])         
          --menMai x l =.(menores x l, maiores x l)
          menMai x [] = ([],[])
          menMai x (a:as) = if x > a then (p,a:q) else (a:p,q)
              where (p,q) = menMai x as

-- {x | x in N && x < 10} = {1,2,3,4,5,6,7,8,9}
-- lista em compreensao      lista em extensao 

-- exemplo [x | x <- [1..10], mod x 2 == 0 ]
      
--mergeSort
mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]-- caso a não esquecer!
mergeSort l   = merge (mergeSort l1) (mergeSort l2) 
    where --l1  = take ( div (length l) 2 ) l --é uma sub-lista de l com (aproximadamente) 1/2 elementos
          --l2  = drop ( div (length l) 2 ) l --com os restantes elementos 
          (l1,l2) = parte l--splitAt (div (length l) 2)
          --l1' = mergeSort l1 
          --l2' = mergeSort l2 
          merge :: Ord a => [a]-> [a] -> [a]
          merge [] b = b
          merge a [] = a
          merge (a:as) (b:bs) = if a < b then a:merge as (b:bs)
                                         else b:merge (a:as) bs  

parte :: [a] -> ([a],[a])
parte []  = ([],[])
parte [x] = ([x],[])
parte (x:x2:xs) = (x:p , x2:q)
    where (p,q) = parte xs



-- a funçao reverse é muito mais rápida com acumuladores
fastRev :: [a] -> [a]
fastRev l = faux [] l
faux ac [] = ac
faux ac (h:t) = faux (h:ac) t

slowRev :: [a] -> [a]
slowRev [] = [] 
slowRev l = last l: slowRev (init l)


-- Função de ordem supeiror : recebem dunçoes como argumento
-- em haskell usa se o map filter...

-- map :: (a->b) ->  [a] -> [b]
dobro :: [Int] -> [Int]
--dobro [] =. []
--dobro (h:t) =. (h*2):dobro t
dobro l = xxx f l
    where f x = 2 * x

triplos :: [Int] -> [Int]
--triplos [] =. [] 
--triplos (h:t) =. (h*3):triplos t
triplos l = xxx f l
    where f x = 3 * x 

mum :: [Int] -> [Int]
--mum [] =. [] 
--mum (h:t) =. (h+1):mum t
mum l = xxx f l
    where f x = x + 1

xxx :: (a -> b) -> [a] -> [b]
xxx muda [] = []
xxx muda (h:t) = muda h:xxx muda t

--xxx muda l = [ muda x | x <- l] 

{-
Igualdade de funçoes 
-}
 

type Matriz = [Linha]
type Linha = [Int]

somaM :: Matriz -> Matriz -> Matriz
--somaM m1 m2 =. zipWith (somalinhas) m1 m2 
somaM m1 m2 = zipWith (zipWith (+)) m1 m2


-------------------------------
--funções infixas (operadores "entre" os operandos) +,*,/,!!
--exp:4 + 5,[1,2,3] !! 2
--podem ser usadas em modo prefixo, englobando-as em ()
--(+) 4 5, (!!) [1,2,3] 2
--
--
--funções prefixas(operador aparece "antes dos operandos") div, mod, elem 
--div 46 6, elem 'a' "anatomia"
--podem ser usadas em modo infixo englobando-as em ``
--46 `div` 6, 'a' `elem` "anatomia"
-------------------------------

soma :: [Int] -> Int
--soma [] = 0
--soma (h:t) = (+) h (soma t)
--soma l = precorre (+) 0 l 
soma = precorre (+) 0 


iSort2 :: Ord a => [a] -> [a]
--iSort2 [] = [] 
--iSort2 (h:t) = inserir h (iSort2 t)
--iSort2 l = precorre inserir [] l 
iSort2 = precorre inserir []
    where inserir x [] = [x]
          inserir x (y:ys) | x <= y = x:y:ys
                           | x > y  = y:inserir x ys

precorre ::(a -> b -> b) -> b -> [a] -> b 
precorre junta v [] = v 
precorre junta v (h:t) = junta h (precorre junta v t) 
-- precorre é um foldr

{-
precorre :: ... -> ... -> [a] -> b

as funçoes foldr sao todas do genero de 
onde dou o acomulador e uma funçao prefixa
func [] = ...
func (h:t)= ... h (func t)
-}


precorreL :: (b -> a -> b) -> b -> [a] -> b
precorreL junta v [] = v
precorreL junta v (h:t) = precorreL junta (junta v h)  t

soma' l = precorre (+) 0 l 
{-
soma' [1,2,3]
= precorre (+) 0 [1,2,3]
= precorre (+) ((+) 0 1) [2,3]
= precorre (+) ((+) ((+) 0 1) 2)
= precorre (+)((+) ((+) ((+) 0 1) 2) 3)
= (+) ((+) ((+) ((+) 0 1) 2)) 3
= ((0+1) +2)+3
-}





