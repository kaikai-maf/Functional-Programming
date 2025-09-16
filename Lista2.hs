-- EX. 1
pertence _ [] = False
pertence x (y:ys)
    | x == y = True
    | otherwise = pertence x ys

-- EX. 2
intersecao [] _ = []
intersecao _ [] = []
intersecao (x:xs) (y:ys)
    |pertence x (y:ys) == True = x : intersecao xs (y:ys)
    |pertence x (y:ys) == False = intersecao xs (y:ys)

--EX. 3
inverso [] = []
inverso (x:xs) = inverso xs ++ x
-- EX. 4

-- EX. 5
-- EX. 6
-- EX. 7
intercalacao [] [] = []
intercalacao [] (y:ys)  = (y:ys) 
intercalacao (x:xs) [] = (x:xs)
intercalacao (x:xs) (y:ys) 
    | x <= y = x : intercalacao xs (y:ys)
    | y <= x = y : intercalacao (x:xs) ys
-- EX. 8
menor [x] = x
menor (x:xs)
        |x < menor xs = x
        | otherwise = menor xs
-- EX. 9
removerElem n [] =[]
removerElem n (x:xs)
    |n == x = xs
    |otherwise = x : removerElem n xs
-- EX. 10
ordenar [] = []
ordenar xs = menor xs : ordenar (removerElem(menor xs) xs)
-- EX. 11
-- EX. 12
-- EX. 13
-- EX. 14
-- EX. 15
-- EX. 16
-- EX. 17
-- EX. 18
-- EX. 19
-- EX. 20
-- EX. 21
-- EX. 22
