sucessor:: Integer -> Integer
sucessor x = x+1

somaLista:: [Integer] -> Integer
somaLista [] = 0
somaLista (a:as) = a + somaLista as

pertence:: Integer -> [Integer] -> Bool
pertence x [] = False
pertence x (a:as) = if(x == a) then True else pertence x as

par:: [(Integer,Integer)] -> [Integer]
par [] = []
par ((a,b):as) = [b] ++ par as

parsoma:: [(Integer,Integer)] -> Integer
parsoma [] = 0
parsoma ((a,b):as) = b + parsoma as

inter:: [Integer] -> [Integer] -> [Integer]
inter [] _ = []
inter _ [] = [] 
inter (x:xs) y = if(pertence x y) then [x] ++ inter xs y else inter xs y

diferenca:: [Integer] -> [Integer] -> [Integer]
diferenca [] _ = []
diferenca _ [] = []
diferenca (x:xs) y = if(pertence x y) then diferenca xs y else [x] ++ diferenca xs y
