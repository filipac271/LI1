module Questao8LI where

{-| Questão8  13/11/2023

A função calcula o total do numero de golos da equipa perdedora numa jornada. Caso o jogo acabe num empate, o total de golos da equipa perdedora é 0.

-} 

type Equipa = String
type Golos = Int
type Jogo = ((Equipa,Golos), (Equipa,Golos))
type Jornada = [Jogo]


f :: Jornada -> Golos
f l = map (+) (\x -> faux l)


faux :: Jornada -> [Int] 
faux (((x,g),(x1,g1)):xs) 
     | g > g1 = g : faux xs
     | g1 > g = g1 : faux xs
     | g == g1 = 0 : faux xs

frecursiva :: Jornada -> Int
frecursiva l = sum (faux l)

{-|

Exemplos

"total de golos da equipa perdedora de dois jogos" :~ 3 :?~ [((equipa1,3),(equipa2,2)),((equipa3,2),(equipa4, 1))]
"total de golos da equipa perdedora de dois jogos" :~ 2 :?~ [((equipa1, 3),(equipa2, 2),(equipa3, 2)(equipa4, 2))]



-}