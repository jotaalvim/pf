data Frac = F Integer Integer

f = F (-33) (-51)
f2 = F 50 (-5)

normaliza :: Frac -> Frac
normaliza (F n d) = F (k * (div n' m )) (div d' m)
    where k = signum n * signum d
          m = mdc n' d'
          n' = abs n
          d' = abs d

mdc :: Integer -> Integer -> Integer
mdc a b
    | a > b = mdc (a-b) b
    | a < b = mdc a (b-a)
    | a==b = b




instance Eq Frac where 
    (F a b) == (F c d) = a*d == c*b

instance Ord Frac where 
    f1 <= f2 = let (F a b) = normaliza f1
                   (F c d) = normaliza f2
               in  a*d <= c*d

instance Show Frac where
    --show :: Frac -> Sring
    show (F a b) = "("++ show a ++"/"++show b ++")"


instance Num Frac where 
    (F a b) + (F c d) = normaliza( F (a*d + c*d) (d*b))
    (F a b) * (F c d) = F (a*c) (b*d)
    negate (F a b) = F (-a) b
    abs (F a b) = F (abs a) (abs b)
    signum (F a b) =F ((signum a) * (signum b)) 1
    fromInteger n = F n 1

--class (Eq a, Show a) => Num a where
--(+), (*), (-) :: a -> a -> a
--negate, abs, signum :: a -> a
--fromInteger :: Integer -> a

sd :: Frac -> [Frac] -> [Frac]
sd f l = filter (> 2*f) l


-- 2 
data Exp a =
     Const a
    |Simetrico (Exp a)
    |Mais  (Exp a) (Exp a)
    |Menos (Exp a) (Exp a)
    |Mult  (Exp a) (Exp a)




infixa :: Show a => Exp a -> String
infixa (Const x) = show x
infixa (Simetrico e) = "- ( "++((infixa e))++" )"
infixa (Mais e1 e2) = '(':' ':(infixa e1)++" + "++ (infixa e2)++" )"
infixa (Menos e1 e2) = '(':' ':(infixa e1)++" - "++ (infixa e2)++" )"
infixa (Mult e1 e2) = '(':' ':(infixa e1)++" * "++ (infixa e2)++" )"

instance Show a => Show (Exp a) where
    show = infixa 

calcula :: Num a => Exp a -> a
calcula (Const x) = x
calcula (Simetrico e) = - (calcula e)
calcula (Mais e1 e2) = (calcula e1)+ (calcula e2)
calcula (Menos e1 e2) = (calcula e1) - (calcula e2)
calcula (Mult e1 e2) =(calcula e1) * (calcula e2) 

{-
instance (Eq a,Num a) => Eq (Exp a) where
    a == b = calcula a == calcula b

instance Num (Exp a) where  
    (+) = Mais
    (*) = Mult
    (-) = Menos
    signum e = Const (signum (calcula e))
    abs e = | calcula e >= 0 = e
            | otherwise = Simetrico e 
    fromInteger n = Const (fromInteger n)

--(+), (*), (-) :: a -> a -> a
--negate, abs, signum :: a -> a
--fromInteger :: Integer -> a

-}

