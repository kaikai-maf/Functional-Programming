
-- Exercício 1
ehTriangulo x y z | x + y > z && x + z > y && y + z > x = True
                  | otherwise = False

--exercicio 2
tipoTriangulo x y z | x == y && y == z = "Equilatero"
                    | x == y || y == z = "isosceles"
                    | x /= y && y /= z = "escaleno"

--exericicio 3
triangulo x y z | ehTriangulo x y z = tipoTriangulo x y z
                | otherwise  = "Nao eh"

ehPar x
    |mod x 2 == 0 =  True
    |otherwise = False
-- exercicio 4
somaPares 0 = 0
somaPares n 
    |n < 2 = 0
    |ehPar n  = n + somaPares (n-1)
    |otherwise = somaPares (n-1)

-- exercicio 5

somaPot2m m 0 = m
somaPot2m m n = somaPot2m m (n-1) + (2^n) * m

-- exericio 6
primo x
    | x > 0 && mod x 2 /= 0 || mod x 2 == 1 = True
    | otherwise =  False

-- exercicio 7
seriePI n |n == 1 = 4/1
          |mod n 2 == 0 = seriePI(n-1)
          |mod (div n 2) 2 == 0 = seriePI(n-2) + (4/fromIntegral n)
          |mod (div n 2) 2 == 1 = seriePI(n-2) - (4/fromIntegral n)
