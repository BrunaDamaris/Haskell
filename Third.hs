itsGraph:: [Integer]->[(Integer,Integer)]->Bool
itsGraph []_= False
itsGraph v [] = True
itsGraph v ((o,d):as) = (itsIn o v  && itsIn d v  && itsGraph v as)

itsIn:: Integer->[Integer]->Bool
itsIn x [] = False
itsIn x (a:as) = if(x == a) then True else itsIn x as

gVertice:: Integer->[(Integer,Integer)]->Integer
gVertice v [] = 0
gVertice v ((o,d):as) = if(v == o)
                        then if(v == d) 
                             then 2 + gVertice v as 
                             else 1 + gVertice v as
                        else if(v == d)
                             then 1 + gVertice v as
                             else gVertice v as

gEVertice:: Integer->[(Integer,Integer)]->Integer
gEVertice v [] = 0
gEVertice v ((o,d):as) = if(v == o)
                         then 1 + gEVertice v as
                         else gEVertice v as

gRVertice:: Integer->[(Integer,Integer)]->Integer
gRVertice v [] = 0
gRVertice v ((o,d):as) = if(v == d)
                         then 1 + gRVertice v as
                         else gRVertice v as
