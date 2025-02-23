module Aula6 where
 
  -- Tentativa de entender o Gloss

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game


type Estado = (Float, Float) -- diz o estado da figura, basicamente onde ela está

type EstadoGloss = (Estado,(Picture, Float)) -- vai ser util para mexer com o tempo

estadoInicial :: Estado
estadoInicial = (0,0)   -- a figura começa no centro

estadoGlossInicial :: Picture -> EstadoGloss
estadoGlossInicial star = (estadoInicial, (star, 0.0)) -- o tempo começa a contar no 0.0 segundos

desenhaEstadoStar :: EstadoGloss -> Picture
desenhaEstadoStar ((x,y), (star, n))  -- vai fazer uma alteração à figura
      | round n `mod` 2 == 0 = star2
      | otherwise = star 
         
  
reageEvento :: Event -> EstadoGloss -> EstadoGloss
reageEvento (EventKey (SpecialKey KeyUp) Down _ _) ((x,y), n) = ((x, y+50), n)
reageEvento (EventKey (SpecialKey KeyDown) Down _ _) ((x,y), n) = ((x, y-50), n)
reageEvento (EventKey (SpecialKey KeyLeft) Down _ _) ((x,y), n) = ((x-50, y), n)
reageEvento (EventKey (SpecialKey KeyRight) Down _ _) ((x,y), n) = ((x+50, y), n)
reageEvento _ s = s  -- ignora qualquer outro evento

reageTempo :: Float -> EstadoGloss -> EstadoGloss
reageTempo n ((x,y), (star,b)) = ((x, y), (star,b+n))






star1 :: Picture          -- the start
star1 = Color blue $ polygon [(0,(-500)),(200,(-600)),(100,(-350)),
                              (300,(-200)), (100,(-200)), 
                              (0,0)
                             ]

star2 :: Picture
star2 = Color rose $ polygon
             [(0,(-500)),((-200),(-600)),((-100),(-350)),
             ((-300),(-200)), ((-100),(-200)), 
             (0,0)
            ] 

star = Pictures [star1, star2]

fr :: Int
fr = 70

window :: Display
window = InWindow "Tentativa de jogo da estrela" (800,600) (0,0)

main :: IO ()
main = 
         play  window                          -- janela onde irá decorrer o jogo
               (white)                 -- cor do fundo da janela
               fr                          -- frame rate
               (estadoGlossInicial star)  -- define estado inicial do jogo
               desenhaEstadoStar               -- desenha o estado do jogo
               reageEvento                 -- reage a um evento
               reageTempo       



{-

ex :: Picture
ex = pictures [translate 100 100 (circleSolid 50),
               rotate 100 (rectangleSolid 100 59) ]


type Estado = (Float,Float)

type EstadoGloss = (Time,Figuras,Estado)
type Figuras = Picture
type Time = Float


estadoInicial :: Picture -> EstadoGloss
estadoInicial star = (0, star, (0,0))

desenhaEstado :: EstadoGloss -> Picture
desenhaEstado (t, star, (x,y)) = rotate 90 star
              

reageEvento :: Event -> EstadoGloss -> EstadoGloss
reageEvento (EventKey (SpecialKey KeyUp) Down _ _ _) 

--reageTempo :: Float -> EstadoGloss 

rotatingstar :: Float -> Picture
rotatingstar angle = rotate angle $ star

update :: Float -> Picture -> Picture
update time _ = rotatingstar time


windowDisplay :: Display
windowDisplay = InWindow "Window" (200, 200) (10, 10)

background :: Color
background = white



star1 :: Picture          -- the start
star1 = Color blue $ polygon [(0,(-50)),(20,(-60)),(10,(-35)),
                              (30,(-20)), (10,(-20)), 
                              (0,0)
                             ]
  
  --(0,0),(100,(-200)),(100,(-200)),
    --         (300,(-200)),(100,(-400)), 
      --       (200,(-600)), (0,(-500))]

star2 :: Picture
star2 = Color rose $ polygon
             [(0,(-50)),((-20),(-60)),((-10),(-35)),
             ((-30),(-20)), ((-10),(-20)), 
             (0,0)
            ] 

star = Pictures [star1, star2]

--rotate :: Float -> Picture -> Picture



ball = Translate (-10) 40  $ Color yellow $ circleSolid 30 

main :: IO ()
main = display windowDisplay white (rotatingstar)


-}

