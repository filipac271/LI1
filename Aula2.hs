module Aula2 where

data Movimento = Norte | Sul | Este | Oeste deriving (Show, Eq)

type Ponto = (Double, Double)

move :: Ponto -> Movimento -> Ponto
move (x,y) m 
 | m == Norte = (x, y+1)
 | m == Sul = (x, y-1)
 | m == Oeste = (x-1, y)
 | m == Este = (x+1, y)
-- Aqui não é preciso o otherwise porque todos os casos já estão definidos

move1 :: Ponto -> Movimento -> Ponto
move1 (x,y) Norte = (x, y+1)
move1 (x,y) Sul = (x, y-1)
move1 (x,y) Oeste = (x-1, y)
move1 (x,y) Este = (x+1,y)



dist :: Ponto -> Ponto -> Double
dist (x1,y1) (x2,y2) = sqrt ((x1-x2)^2 + (y1-y2)^2)


-- Exercicio 2
mudaO :: Ponto -> Double -> Ponto
mudaO (x3,y3) l = (x3, y3-l)

mudaO' :: Ponto -> Double -> Ponto
mudaO' (x4,y4) l1 = (x4-l1/2, y4-l1/2)

-- Exercicio 3

type Velocidade = Double
type Tempo = Double
move' :: Ponto -> Velocidade -> Tempo -> Ponto
move' (a,b) v t = (a+v*t, b)

-- Ex 4
movev :: Ponto -> Velocidade -> Tempo -> Ponto
movev (a1,b1) v1 t1 = (a1, b1+v1*t1)

-- Ex 5
type Velocidade1 = (Double, Double)
move2 :: Ponto -> Velocidade1 -> Tempo -> Ponto
move2 (a2,b2) (v2,v3) t2 = (a2+v2*t2, b2+v3*t2)

-- Ex 6
data Figura = Circulo Ponto Double | Retangulo Ponto Ponto
                     deriving (Show,Eq)

dentro :: Figura -> Ponto -> Bool
dentro (Circulo (c1,c2) r) (x5,y5) = if ((x5-c1)^2 + (y5-c2)^2 == r^2) then True else False
dentro (Retangulo (l1,l3) (l4,l5)) (x6,y6) = if( (l1 <= x6 && x6 <= l4) && (l3 <= y6 && y6 <= l5)) 
    then True else False

dentro' :: Figura -> Ponto -> Bool
dentro' (Circulo c r1) p = dist c p <= r1
dentro' (Retangulo (a3,b3) (a4,b4) (x7,y7))
  let maxa = max a3 a4
      maxb = max b3 b4
      mina = min a3 a4
      minb = min b3 b4
  in  x7 <= maxa && x7 >= mina && y7 <= maxb && y7 >= minb

-- Ex 7 -> foi feito em pf

-- Ex 8

type Nome = String
type Coordenada = (Double, Double)
data Movimento = Norte | Sul | Este | Oeste deriving (show, Eq) -- norte, sul, este, oeste
type Movimentos = [Movimento]
data PosicaoPessoa = Pos Nome Coordenada deriving (Show, Eq)

posicao :: PosicaoPessoa -> Movimentos -> PosicaoPessoa
posicao Pos n (p1, p2) []










