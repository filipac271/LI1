module Aula6 where
 

  -- Tentativa de entender o Gloss

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

windowDisplay :: Display
windowDisplay = InWindow "Window" (200, 200) (10, 10)

main :: IO ()
main = display windowDisplay white (Circle 80)





