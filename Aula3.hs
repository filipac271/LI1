module Aula3 where

import Data.Char

-- ^adiciona um valor dado a uma lista de inteiros       

maisn :: Int -> [Int] -> [Int]
maisn _ [ ] = [ ]
maisn n (x:xs) = (n + x) : maisn n xs

rstrings :: Char -> [String] -> [String]
rstrings _ [ ] = [ ]
rstrings r (h:t) 
        | h == [ ] = rstrings r t
        | r == head h = rstrings r t
        | otherwise = h : rstrings r t

parn :: Int -> [(Int, Int)] -> [(Int,Int)]
parn _ [ ] = [ ]
parn p ((h1,h2): t1) = (p+h1,h2) : parn p t1

maiorpar :: [(Int, Int)] -> Int
maiorpar [ ] = 0
maiorpar [(x,y)] = y
maiorpar ((x1,x2): (y1,y2): zx) 
        | x2 > y2 = maiorpar ((x1,x2):zx)
        | otherwise = maiorpar ((y1,y2):zx)

-- recebe digitos
nextdigit :: Char -> Char
nextdigit c 
      | chr (ord c) < chr (ord '9') = chr ((ord c) +1)
      | chr (ord c) == chr (ord '9') = chr ((ord '9') -9)

--nextvogal :: [Char] -> [Char]
--nextvogal (x:xs) = if chr (ord x) > chr (ord 'o') then ((ord 'o') - 3)
                    --else chr ((ord x) +1) 


nextvogal' :: Char -> Char
nextvogal' c = aux c "aeioua"
  where aux :: Char -> String -> Char
        aux c s 
          | c == head s = head (tail s)
          | otherwise = aux c (tail s)



deslocaEsquerda :: [a] -> [a]
deslocaEsquerda [ ] = [ ]
deslocaEsquerda (x:xs) = xs ++ [x]

deslocaEsquerdaN :: [a] -> Int -> [a]
deslocaEsquerdaN [ ] _ = [ ]
deslocaEsquerdaN l 0 = l
deslocaEsquerdaN l n = deslocaEsquerdaN (deslocaEsquerda l) (n-1)

deslocaDireita :: [a] -> [a]
deslocaDireita [ ] = [ ]
deslocaDireita l = (last l): (init l)

{-
>>> deslocaDireita [1,2,3,4] 2
[4,3,1,2]
-}

deslocaDireitaN :: [a] -> Int -> [a]
deslocaDireitaN [ ] _ = [ ]
deslocaDireitaN l 0 = l
deslocaDireitaN l n = deslocaDireitaN (deslocaDireita l) (n-1)


{-deslocad :: Int -> [a] -> [a]
deslocad _ [ ] = [ ]
deslocad n (h:t:ts)
         | n < 0 = (h:t:ts)
         | n == 0 = (t:ts:h)
         | n == 1 = (ts:h:t)
         | n > 1 = deslocad (n-2) ts ++ h ++ t 


deslocae :: Int -> [a] -> [a]
deslocae _ [ ] = [ ]
deslocae n (x:xs)
         | n < 0 = (x:xs)
         | n == 0 = (last xs : x : init xs)
         | n > 0 = (last xs : deslocae (x :(init xs)))
-}


type Nome = String
type Coordenada = (Int, Int)
data Movimento= N | S | E | W deriving (Show,Eq) -- norte, sul, este, oeste
type Movimentos = [Movimento]
data PosicaoPessoa = Pos Nome Coordenada deriving (Show,Eq)

posicao:: PosicaoPessoa -> Movimentos -> PosicaoPessoa
posicao p [] = p        
posicao (Pos nome (x,y)) (m:lm) 
               | m == N = posicao (Pos nome (x,y+1)) lm 
               | m == S = posicao (Pos nome (x,y-1)) lm 
               | m == E = posicao (Pos nome (x+1,y)) lm 
               | m == W = posicao (Pos nome (x-1,y)) lm 

posicoesM:: [PosicaoPessoa] -> Movimento -> [PosicaoPessoa]
posicoesM [ ] _ = [ ]
posicoesM (p:lp) m = posicao p [m] : posicoesM lp m

posicoesM':: [PosicaoPessoa] -> Movimento -> [PosicaoPessoa]
posicoesM' [ ] _ = [ ]
posicoesM'((Pos nome (x,y)): lp) N = (Pos nome (x,y+1)): posicoesM' lp N
posicoesM'((Pos nome (x,y)): lp) S = (Pos nome (x,y-1)): posicoesM' lp S
posicoesM'((Pos nome (x,y)): lp) E = (Pos nome (x+1,y)): posicoesM' lp E
posicoesM'((Pos nome (x,y)): lp) W = (Pos nome (x-1,y)): posicoesM' lp W 
 
posicoesMs:: [PosicaoPessoa] -> Movimentos -> [PosicaoPessoa]
posicoesMs lposicoes [ ] = lposicoes
posicoesMs lposicoes (m:lm) = posicoesMs (posicoesM lposicoes m) lm

posicoesMs':: [PosicaoPessoa] -> Movimentos -> [PosicaoPessoa]
posicoesMs' [ ] _ = [ ]
posicoesMs' (p:ps) lm = posicao p lm : posicoesMs' ps lm

{- coordenadaNorte :: [PosicaoPessoa] -> Coordenada
coordenadaNorte ((Pos nome (x,y)): (Pos nome1 (x1,y1)): lp)
                | y > y1 = y
                | y1 > y = y1
                | otherwise = coordenadaNorte (Pos nome1(x1,y1):lp) -}

coordenadaNorte :: [PosicaoPessoa] -> Coordenada
coordenadaNorte [Pos n (x,y)] = (x,y)
coordenadaNorte ((Pos n (x,y)): lp) =
        let (xn,yn) = coordenadaNorte lp
        in if y>= yn then (x,y) else (xn,yn)

coordenadaNorte' :: [PosicaoPessoa] -> Coordenada
coordenadaNorte' [Pos n (x,y)] = (x,y)
coordenadaNorte' ((Pos n (x,y)): (Pos n1 (x1,y1)): lp)
              | y > y1 = coordenadaNorte' ((Pos n (x,y)):lp)
              | otherwise = coordenadaNorte' ((Pos n1 (x1,y1)):lp)

pessoasNorte' :: [PosicaoPessoa] -> Int
pessoasNorte' [Pos nome (x,y)] = y
pessoasNorte' ((Pos nome (x,y)):t) =
              let yn =  pessoasNorte' t
              in max y yn
-- esta função utiliza a recursividade sobre a cauda. Vai ver qual é o maior da cauda e depois compara à tail. 


pessoasNorte:: [PosicaoPessoa] -> [Nome] 
pessoasNorte [ ] = [ ]
pessoasNorte l =
            let yn = pessoasNorte' l
            in getPessoas l yn 


getPessoas :: [PosicaoPessoa] -> Int -> [Nome]
getPessoas ((Pos nome (x,y)):t) yn 
            | y == yn = nome : getPessoas t yn
            | y /= yn = getPessoas t yn

pessoaNorte :: [PosicaoPessoa] -> [Nome]
pessoaNorte  [ ] = [ ]
pessoaNorte ((Pos nome(x,y)):t) = pessoaNorteAux t


pessoaNorteAux :: [PosicaoPessoa] -> Int -> [Nome] ->[Nome]
pessoaNorteAux (Pos nome1(x1,y1):t) yn ln
         | y1 < yn = pessoaNorteAux t yn ln
         | y1 == yn = pessoaNorteAux t yn (nome1:ln)
         | y1 > yn = pessoaNorteAux t y1 [nome1]