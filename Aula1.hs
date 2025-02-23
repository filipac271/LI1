module Aula1 where
    
areaQ :: Int -> Int
areaQ lado = lado*lado

perimetro :: Float -> Float -> Float
perimetro comp largura = 2*(comp + largura)

pertenceS :: Char -> String -> Bool
pertenceS x lista = elem x lista

removelista :: [a] -> [a]
removelista l = if (mod (length l) 2) == 0
                   then tail l
                   else init l

primUlt :: [a] -> (a,a)
primUlt lista = (head lista, last lista)

parlistas :: ([a], [a]) -> (a, [a])
parlistas (xs, ys) = (head xs, ys) 
        
somapar :: (Int,Int) -> (Int,Int) -> (Int,Int)
somapar (x,y) (w,z) = (x+w , y+z)

somapar' :: (Int,Int) -> (Int,Int) -> (Int,Int)
somapar' (x,y) (w,z) = if (mod (x+w) 2) ==0 
                          then (0, y+z)
                          else (x+w , y+z)
