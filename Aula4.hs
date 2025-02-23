module Aula4 where

-- recebe lista de 10 notas e a nota do projeto
notafinal ::[Float] -> Float -> Float
notafinal lnac np = (sum lnac)*0.4 + np*0.6

trocaPrimUlt :: [a] -> [a]
trocaPrimUlt [ ] = [ ]   -- uma lista vazia, náo é uma lista sem anda, é uma lista singular
trocaPrimUlt [x] = [x]
trocaPrimUlt (x:xs) = (last xs : init xs) ++ [x]

-- Ex 2

trocalinhas :: [[a]] -> [[a]]
trocalinhas m = trocaPrimUlt m 

trocacolunas:: [[a]] -> [[a]]
trocacolunas [ ] =[ ]
trocacolunas (l:m) = (trocaPrimUlt l) : trocacolunas m 

mat :: [[Int]]
mat = [[1,2,3],[4,5,6], [7,8,9]]

soma :: [Int] -> [Int] -> [Int]
soma (x:xs) [ ] = [ ]
soma [ ] (y:ys) = [ ]
soma (x:xs) (y:ys) = (x + y) : (soma xs ys)

posicao :: Eq a => [a] -> a -> Int
posicao [ ] _ = -1
posicao l n = posAux  l n 0
        
posAux :: Eq a => [a] -> a -> Int -> Int
posAux [ ] _ _ = -1
posAux (x:xs) y p
         | x == y = p
         | otherwise = posAux xs y (p+1)



{-posicao :: Int -> [a] -> Int
posicao _ [ ] = -1
posicao n (h:t)
        | n == h = 0
        | otherwise = posicao n t

Sub :: Int -> a -> [a] -> [a]
Sub _ _ [ ] -> [ ]
Sub n m (x:xs)
    | n == 0 = (m:xs)
    | otherwise x ++ Sub n m xs -}