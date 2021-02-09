avalia :: FilmesAval -> IO FilmesAval 
avalia m@((x,xs):t) = do
    putStrLn "Insira o título do filme" 
    y <- getLine
    putStrLn "Insira avaliação"
    z <- getLine 
    if igual y m == True then return ((x,(xs++[z])):t)
    else avalia t 

igual :: Titulo -> FilmesAval -> Bool
igual x [] = False 
igual x (((tit,real,l,gen,ano),ys):t)
    | x == tit = True 
    | otherwise = igual x t 
