module Ficha3 where

import Ficha1

type Etapa = (Hora1,Hora1)
type Viagem = [Etapa]

etapaValida :: Etapa -> Bool
etapaValida (h1,h2) = (horaPosterior2 h2 h1) && horaValida2 h1 && horaValida2 h2

viagemValida :: Viagem -> Bool
viagemValida [] = True
viagemValida [e] = etapaValida e
viagemValida (e1:e2:v) = (etapaValida e1) && (horaPosterior2 (fst e2) (snd e1)) && viagemValida (e2:v)

partidaChegada :: Viagem -> (Hora1,Hora1)
partidaChegada [] = error "Nao ha etapas"
partidaChegada (e1:rv) = (fst e1, (snd . last) rv) 

tempoViagem :: Viagem -> Hora1
tempoViagem [] = H 0 0
tempoViagem v = (minToHora2 . sum) (map (uncurry(difHoras2)) v)

tempoEspera :: Viagem -> Hora1
tempoEspera v = tempoEsperaAux v 0
                where tempoEsperaAux ((h1,h2):(h3,h4):rv) ac = tempoEsperaAux ((h3,h4):rv) (ac + (difHoras2 h3 h2))
                      tempoEsperaAux (e:rv) ac = minToHora2 ac

tempoTotalViagem :: Viagem -> Hora1
tempoTotalViagem v = (minToHora2.sum) (map (horaToMin2) [tempoViagem v, tempoEspera v])

--Exercicio 2
type Poligonal = [Ponto]

--a 
comprimento :: Poligonal -> Double
comprimento (p1:p2:lp) = distpontos p1 p2 + comprimento (p2:lp)
comprimento _ = 0

fechada :: Poligonal -> Bool
fechada [] = True
fechada l 
 | head l == last l && length l > 1 = True
 | otherwise = False 

--c         fechado
triangula :: Poligonal -> [Figura]
triangula lp = parte2 (init lp)
 where parte2 (p1:p2:p3:t) = (Triangulo p1 p2 p3): parte2 (p1:p3:t)
       parte2 _ = []

areaLinha :: Poligonal -> Double
areaLinha [] = 0
areaLinha l 
 | fechada l = sum (map (area) (triangula l))
 | otherwise = error "Nao e uma linha poligonal fechada e convexa"

mover :: Poligonal -> Ponto -> Poligonal
mover l p 
 | fechada l == False || length l < 4 = error "Impossivel"
 | otherwise = map (moveponto (coefMov (head l) p)) l
 where coefMov :: Ponto -> Ponto -> (Double,Double)
       coefMov p1 p2 = ((posx p2)-(posx p1), (posy p2) - (posy p1))
       moveponto :: (Double,Double) -> Ponto -> Ponto
       moveponto (x,y) p = (Cartesiano ((posx p)+ x) ((posy p) + y))


--Exercicio 3
data Contacto = Casa Integer | Trab Integer | Tlm Integer | Email String deriving Show
type Agenda = [(Nome, [Contacto])]

--a
acrescEmail :: Nome -> String -> Agenda -> Agenda
acrescEmail n em [] = [(n,[Email em])]
acrescEmail n em ((n1,lc):ag)
 | n == n1 = (n,(Email em):lc):ag
 | n /= n1 = (n1,lc) : acrescEmail n em ag

--b
verEmails :: Nome -> Agenda -> Maybe [String]
verEmails _ [] = Nothing
verEmails n ((n1,lc):ag) 
 | n == n1 = Just (getEmail lc)
 | n /= n1 = verEmails n ag

getEmail :: [Contacto] -> [String]
getEmail [] = []
getEmail ((Email e):xs) = e:getEmail xs
getEmail (_:xs) = getEmail xs 

--c
consTelefs :: [Contacto] -> [Integer] 
consTelefs [] = []
consTelefs ((Trab n):lc) = n : consTelefs lc
consTelefs ((Casa n):lc) = n : consTelefs lc
consTelefs ((Tlm n):lc) = n : consTelefs lc
consTelefs (_:lc) = consTelefs lc

--d
casa :: Nome -> Agenda -> Maybe Integer
casa n [] = Nothing
casa n ((n1,lc):ag)
 | n == n1 = getCasa lc
 | n /= n1 = casa n ag

getCasa :: [Contacto] -> Maybe Integer
getCasa [] = Nothing
getCasa ((Casa n):lc) = Just n
getCasa (_:lc) = getCasa lc


--Exercicio 4
type Dia = Int
type Mes = Int
type Ano = Int
type Nome = String
data Data = D Dia Mes Ano deriving Eq
type TabDN = [(Nome,Data)]

tab :: TabDN
tab = [("T-Bag", D 12 7 2002), ("Hermelinda", D 27 9 1972), ("Puta da Tua Prima", D 6 9 1969)]

procura :: Nome -> TabDN -> Maybe Data
procura a [] = Nothing
procura a ((n,d):t) | a == n = Just d
                    | otherwise = procura a t

idade :: Data -> Nome -> TabDN -> Maybe Int
idade d a [] = Nothing
idade d1 a ((n,d):t) | a == n = (comparaidade d1 d)
                     | otherwise = idade d1 a t
                     where comparaidade (D d1 m1 a1) (D d m a) 
                            | a1 < a    = Nothing
                            | a == a1   = Just 0
                            | m == m1   = if d > d1 then Just (a1-a-1) else Just (a1-a)
                            | m > m1    = Just (a1-a-1)
                            | otherwise = Just (a1-a)

anterior :: Data -> Data -> Bool
anterior (D d1 m1 a1) (D d m a) 
 | a1 < a    = True
 | a == a1   = anterior (D d1 m1 (a1+1)) (D d m a)
 | a1 > a    = False
 | m == m1   = if d > d1 then True else False
 | m > m1    = True
 | otherwise = False

ordena :: TabDN -> TabDN
ordena [] = []
ordena t 
 | (map (\x -> 1) (filter (\x -> anterior (snd(head t)) (snd x)) t)) == [] = ordena (tail t) ++ [head t]
 | otherwise = ordena ((tail t) ++ [head t])

porIdade:: Data -> TabDN -> [(Nome,Int)]
porIdade da t = map (\(n,d) -> (n, comparaidade da d)) ((reverse.ordena) t)
 where comparaidade (D d1 m1 a1) (D d m a) 
                            | a1 < a    = -1 - (comparaidade (D d m a) (D d1 m1 a1))
                            | a == a1   = 0
                            | m == m1   = if d > d1 then (a1-a-1) else (a1-a)
                            | m > m1    = (a1-a-1)
                            | otherwise = (a1-a)


--Exercício 5

data Movimento = Credito Float | Debito Float 
--data Data = D Int Int Int deriving Show
data Extracto = Ext Float [(Data, String, Movimento)] 

--a
extValor :: Extracto -> Float -> [Movimento]
extValor (Ext _ lm) v = filtraMov lm v

filtraMov :: [(Data, String, Movimento)] -> Float -> [Movimento]
filtraMov [] _ = []
filtraMov ((_,_,Debito x):t) v
 | x > v = (Debito x):filtraMov t v 
 | otherwise = filtraMov t v
filtraMov ((_,_,Credito x):t) v
 | x > v = (Credito x):filtraMov t v
 | otherwise = filtraMov t v

filtraMov2 :: [(Data, String, Movimento)] -> Float -> [Movimento]
filtraMov2 [] _ = []
filtraMov2 ((_,_,m):t) v = 
    case m of 
        (Debito x) -> if x > v then m:filtraMov2 t v else filtraMov2 t v
        (Credito x) -> if x > v then m:filtraMov2 t v else filtraMov2 t v

--b
filtro :: Extracto -> [String] -> [(Data, Movimento)]
filtro (Ext _ lm) ld = aux ld lm
 where aux :: [String] -> [(Data,String,Movimento)] -> [(Data, Movimento)]
       aux ls [] = []
       aux ls ((d,s,m):t)
        | elem s ls = (d,m):aux ls t
        | otherwise = aux ls t
{-- Versao Diferente
 aux ls [] = []
 aux (s1:ls) lm = (procura s1 lm) ++ aux ls lm
 procura s [] = []
 procura s ((d,s,m):t)
  | s == s1 = [(d,m)]++procura s t
  | s /= s1 = procura s t --}

--c
creDeb:: Extracto -> (Float, Float)
creDeb (Ext v1 lm) = parte lm
 where parte :: [(Data,String,Movimento)] -> (Float,Float)
       parte [] = (0,0)
       parte ((_,_,m):t) =
        let(sc,sd) = parte t
        in case m of 
            (Credito x) -> (x+sc , sd)
            (Debito x) -> (sc , x+sd)

--d
saldo :: Extracto -> Float
saldo e@(Ext v1 lm) = let (sc,sd) = creDeb e
                      in v1 + sc - sd

instance Ord Data where
    compare a b | a == b = EQ
                | anterior a b = LT
                | otherwise = GT

instance Show Data where
    show (D d m a) = show a ++ "/" ++ show m ++ "/" ++ show d