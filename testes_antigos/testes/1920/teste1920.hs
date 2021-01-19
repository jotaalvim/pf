


{-que retorna a lista resultante de remover da primeira
lista os elementos que não pertencem à segunda.-}

intersect :: Eq a => [a] -> [a] -> [a]
intersect [] [] = []
intersect (h:t) l = if elem h l then h: intersect t l
                    else intersect t l

intersect2 l l2 = [ x | x <- l , elem x l2]

intersect3 l l2 = filter (\x -> elem x l2 ) l



{-que calcula a lista dos sufixos de uma lista. Por exemplo, tails [1,2,3]
corresponde a [[1,2,3],[2,3],[3],[]].-}

tails :: [a] -> [[a]]
tails [] = [[]]
tails l  = l : tails (tail l)

tails2 l = [ drop x l | x <- [0..length l]] 


type ConjInt = [Intervalo]
type Intervalo = (Int,Int)

{-que, dado um conjunto, dá como resultado a lista
dos elementos desse conjunto.-}

elems :: ConjInt -> [Int]
elems l = concat $ map (uncurry enumFromTo) l

elems2 l = concat [  [x..y]  | (x,y) <- l]
{--geraconj [1,2,3,4,7,8,19,21,22,23] = [(1,4),(7,8),(19,19),(21,23)].-}



geraconj :: [Int] -> ConjInt
geraconj [] = []
geraconj l@(h:t)  = (h,m) : geraconj (drop (m-h) l) 
    where (h,m)   = aux h h l

aux :: Int -> Int -> [Int] -> (Int,Int)
aux h m [] = (h,m)
aux h m (h2:t) = if h2 - m == 1 then aux h h2 t 
                 else (h,m) 


data Contacto = Casa Integer
              | Trab Integer
              | Tlm Integer
              | Email String
              deriving (Show)

type Nome = String
type Agenda = [(Nome, [Contacto])]

agen = [("Manela",[Email "manela@gmail.com",Tlm 934662832,Email "222@gmail.com"])] 


acrescEmail :: Nome -> String -> Agenda -> Agenda
acrescEmail no email agenda = if  null pessoa then (no,[Email email ]):agenda
                              else pessoa ++ naopessoa 
    where pessoa    = [(nome,(Email email):l) | (nome,l) <- agenda,nome == no]
          naopessoa = [(nome,(Email email):l) | (nome,l) <- agenda,nome /= no]


verEmails :: Nome -> Agenda -> Maybe [String]
verEmails no agenda = if null pessoa 
                      then Nothing 
                      else Just (fm ( snd (head pessoa )))
    where pessoa = [(nome,l) | (nome,l) <- agenda,nome == no]
          fm [] = []
          fm (Email x:t) = x : fm t
          fm ( _:t) = fm t   

consulta :: [Contacto] -> ([Integer],[String])
consulta [] = ([],[])
consulta (Email x:t) = (p,x:q)
    where (p,q) = consulta t
consulta (Tlm x:t) = (x:p,q)
    where (p,q) = consulta t
consulta (Casa x:t) = (x:p,q)
    where (p,q) = consulta t
consulta (Trab x:t) = (x:p,q)
    where (p,q) = consulta t


consultaIO :: Agenda -> IO ()
consultaIO agenda = do 
    con <- getLine
    let pessoa = [(nome,l) | (nome,l) <- agenda,nome == con]
    putStr $ show $ snd $ head pessoa


data RTree a = R a [RTree a] deriving (Show, Eq)

ar = R 1 [R 2 [], 
          R 3 [R 4 [R 5 [], 
                    R 6 []]],
          R 7 []]

paths :: RTree a -> [[a]]
paths (R x []) = [[x]]
paths (R x l ) = map (x:) $ concat $ map paths l

-- [  [[2]], [[3,4,5],[3,4,6]], [[7]]  ]

paths2 :: RTree a -> [[a]]
paths2 (R x []) = [[x]]
paths2 (R x l)  =     [ x : f    | f <- lf]
    where lf = concat [ paths2 f | f <- l ]





