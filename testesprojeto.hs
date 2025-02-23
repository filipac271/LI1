module Testesprojeto where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

import Graphics.Gloss.Juicy (loadJuicyPNG, loadJuicyJPG)

import System.Exit
import Data.Char
import Data.List

import Data.Maybe

import GHC.Float 


data Bloco
  = Escada       -- ^ Permite ao jogador mover-se verticalmente
  | Plataforma   -- ^ Bloco sólido que pode ser utilizado como superfície
  | Alcapao      -- ^ Bloco que desaparece após ser atravessado pelo jogador
  | Vazio        -- ^ Espaço
  deriving (Ord, Eq, Read, Show)

-- | Mapa de um 'Jogo', composto por uma posição e direção inicial, posição final e uma matriz de blocos.
data Mapa =
  Mapa (Posicao, Direcao) Posicao [[Bloco]]
  deriving (Eq, Read, Show)

type Hitbox = (Posicao, Posicao)

-- | Vetor velocidade.
type Velocidade = (Double, Double)

-- | Posicao no 'Mapa'.
type Posicao = (Double, Double)

-- | Períodos de tempo.
type Tempo = Double

-- | Direção de um 'Personagem' no 'Mapa'.
data Direcao
  = Norte
  | Sul
  | Este
  | Oeste
  deriving (Ord, Eq, Read, Show)

altura :: Float
altura = 190

comprimento :: Float
comprimento = 320

largura :: Float
largura = 10

type Coordenadass = (Float,Float)

type Blocos = [(Bloco,Picture)]

--type PicBlocos = (Bloco) --nos varios sitios tem coordenadas, mas eu não entendo porque 

getImagem :: Bloco -> Blocos -> Picture
getImagem k d = fromJust $ lookup k d -- ja entendo


desenhaMapa :: Float -> Float -> Blocos -> Mapa -> [Picture]
desenhaMapa c l imgs (Mapa((a,b),d)(x1,y1)(x:xs)) = linha1 ++ restol
              where linha1 = desenhaLinha c l imgs x
                    restol = desenhaMapa c (l - largura) imgs (Mapa((a,b),d)(x1,y1)(xs))
desenhaMapa _ _ _ _ = [ ]

desenhaLinha :: Float -> Float -> Blocos -> [Bloco] -> [Picture]
desenhaLinha c l imgs (x:xs) = bloco1 : restob
             where bloco1 = desenhaBloco c l imgs x
                   restob = desenhaLinha (c + largura) l imgs xs


desenhaBloco :: Float -> Float -> Blocos -> Bloco ->  Picture
desenhaBloco c l imgs ((x)) = Translate c l $ getImagem x imgs


data Imagens = Imagens {
    --mario :: Picture,
    escada :: Picture,
    --estrela1 :: Picture,
   -- estrela2 :: Picture,
    --fantasma1 :: Picture,
    --fantasma2
    --macacomalvado :: Picture,
    plataforma :: Picture,
    alcapao :: Picture,
    vazio :: Picture
}


{- type Imagem = [( Bloco, Picture)] de forma a atribuit um nome a uma imagem e depois vai buscar 
tmb dá para fazer com o data , 
 
 type imagensblocos = [(Bloco, picture)]


-}


--faltam imagens adicionar depois

carregaImagens :: IO Imagens
carregaImagens = do
    --Just mario <- loadJuicyPNG "mariofrente.jpg"
    Just escada <- loadJuicyJPG "escada.jpg"
    Just plataforma <- loadJuicyJPG "bloco.jpg"
    Just alcapao <- loadJuicyJPG "alcapao.jpg"
    Just vazio <- loadJuicyJPG "vazio.jpg"

    return (Imagens escada plataforma alcapao vazio) --tiraste o mario

fr:: Int
fr = 50

dm :: Display
dm = InWindow
       "Novo Jogo"  -- título da janela
       (400, 400)   -- dimensão da janela
       (0,0)        -- posição no ecran

main :: IO ()
main = do 
    desenhaMapa
   