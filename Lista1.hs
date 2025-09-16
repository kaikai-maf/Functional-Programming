
-- EX.1: verify if given numbers form a triangle
ehTriangulo x y z | x + y > z && x + z > y && y + z > x = True
                  | otherwise = False

-- EX.2: classify what kind of triangle given numbers form
tipoTriangulo x y z | x == y && y == z = "Equilatero"
                    | x == y || y == z = "isosceles"
                    | x /= y && y /= z = "escaleno"

-- EX.3: now, we merge both of the previous funcions to verify wether it is or not a triangle, also classifying what kind it is
triangulo x y z | ehTriangulo x y z = tipoTriangulo x y z
                | otherwise  = "Nao eh"
-- EX. 4: 
--verify if given number is even
ehPar x
    |mod x 2 == 0 =  True
    |otherwise = False

-- sum of all numbers up to n
somaPares 0 = 0
somaPares n 
    |n < 2 = 0
    |ehPar n  = n + somaPares (n-1)
    |otherwise = somaPares (n-1)

-- EX. 5: sum of 2 powered up to n, times m

somaPot2m m 0 = m
somaPot2m m n = somaPot2m m (n-1) + (2^n) * m

-- EX. 6: verify wether given number is prime or not
primo x
    | x > 0 && mod x 2 /= 0 || mod x 2 == 1 = True
    | otherwise =  False

-- EX. 7: 
seriePI n |n == 1 = 4/1
          |mod n 2 == 0 = seriePI(n-1)
          |mod (div n 2) 2 == 0 = seriePI(n-2) + (4/fromIntegral n)
          |mod (div n 2) 2 == 1 = seriePI(n-2) - (4/fromIntegral n)
