somalista:: [Integer] -> Integer
somalista [] = 0
somalista (a:as) = a + somalista as

listas:: [[Integer]] -> [Integer]
listas [] = []
listas (a:as) = [somalista (a)] ++ listas as

listaspares:: [(Integer,Integer)] -> [Integer]
listaspares [] = []
listaspares ((a,b):as) = [a] ++ listaspares as

paressoma:: [(Integer,Integer)] -> [Integer]
paressoma [] = []
paressoma ((a,b):as) = [a+b] ++ paressoma as

pertence:: Integer -> [Integer] -> Bool
pertence x [] = False
pertence x (a:as) = if(x == a) then True else pertence x as

inter:: [Integer] -> [Integer] -> [Integer]
inter [] _ = []
inter _ [] = [] 
inter (x:xs) y = if(pertence x y) then [x] ++ inter xs y else inter xs y

diferenca:: [Integer] -> [Integer] -> [Integer]
diferenca [] _ = []
diferenca _ [] = []
diferenca (x:xs) y = if(pertence x y) then diferenca xs y else [x] ++ diferenca xs y

uniao:: [Integer] -> [Integer] -> [Integer]
uniao [] _ = []
uniao _ [] = []
uniao (x:xs) (y:ys) = [x,y] ++ uniao xs ys

paresentrelistas:: [Integer] -> [Integer] -> [(Integer,Integer)]
paresentrelistas [] _= []
paresentrelistas _ []= []
paresentrelistas (x:xs) (y:ys) = [(x,y)] ++ paresentrelistas xs ys
