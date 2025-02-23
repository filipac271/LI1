module Questao9e10 where

type Coordenada = (Int,Int)
type Comprimento = Int
type Largura = Int

data Movimento = E | W | N | S  
              deriving (Show, Eq)

type Mapa = (Comprimento, Largura)
             -- Largura: dimensao do lado paralelo ao eixo dos y

{-| A função sequencia recebe duas coordenadas e calcula a sequencia de movimentos para isso utiliza duass funções auxiliares, uma que calcula a sequencia de movimentos no eico dos x e outra no eixo dos y
-}


sequencia :: Coordenada -> Coordenada -> [Movimento]
sequencia (x1,y1) (x2,y2) = sequencia1 (x1,y1) (x2,y2) ++ sequencia2 (x1,y1) (x2,y2)
dequencia' :: Coordenada -> Coordenada -> [Movimento] 
dequencia' (x1,y1)(x2,y2) 
     |N =            

sequencia1 :: Coordenada -> Coordenada -> [Movimento]
sequencia1 (x1,y1) (x2,y2)
          | n == 0 = [ ] 
          | n < 0 = [E] ++ sequencia1 (x1,y1) ((x2-1),y2)
          | n > 0 = [W] ++ sequencia1 ((x1-1),y1) (x2,y2)
        where n = (x1-x2)  

sequencia2 :: Coordenada -> Coordenada -> [Movimento]
sequencia2 (x1,y1) (x2,y2)    
      | m == 0 = [ ] 
      | m < 0 = [N] ++ sequencia2 (x1,y1) (x2,(y2-1))
      | m > 0 = [S] ++ sequencia2 (x1, (y1-1)) (x2,y2)
     where m = (y1-y2)
{-|

testes: 

"sequencia" ~: [E,N] ~=? sequencia (1,1) (2,2)
"sequencia" ~: [W,S] ~=? sequencia (2,2) (1,1)

-}
retiraR :: [Movimento] -> [Movimento]
retiraR (x:xs)
        | (x:xs) == (N:S:xs) = retiraR xs
        | (x:xs) == (S:N:xs) = retiraR xs
        | (x:xs) == (W:E:xs) = retiraR xs
        | (x:xs) == (E:W:xs) = retiraR xs
        | otherwise = [x] ++ retiraR xs

{-|
testes:


"retira movimentos " ~: [S,E] ~=? retiraR [N,S,S,E]
"retira movimentos" ~: [N,E] ~=? retiraR [W,E,N,E]

-}