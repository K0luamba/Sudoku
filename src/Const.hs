module Const where

import Graphics.Gloss.Interface.Pure.Game

-- константы --

configPath1 :: FilePath
configPath1 = "config1.txt"

configPath2 :: FilePath
configPath2 = "config2.txt"

configPath3 :: FilePath
configPath3 = "config3.txt"

{-tableExample :: Table 
tableExample = 
    Table {content = [ createQuad [2,3,8,7,5,9,4,1,6]
                     , createQuad [9,6,5,4,1,3,2,7,8]
                     , createQuad [7,1,4,6,8,2,9,5,3]
                     , createQuad [9,4,5,6,8,7,3,2,1]
                     , createQuad [1,3,6,5,2,4,8,9,7]
                     , createQuad [2,7,8,1,3,9,4,6,5]
                     , createQuad [1,6,2,5,7,4,8,9,3]
                     , createQuad [3,5,9,6,8,2,7,4,1]
                     , createQuad [8,4,7,3,9,1,5,2,6]
                     ]
          } -}

display :: Display --по сути определение размеров экрана
display = FullScreen

bgColor :: Color --фон
bgColor = greyN 0.9

stepsPerSecond :: Int
stepsPerSecond = 60

cellSize :: Float
cellSize = 24

fieldSize@(fieldWidth, fieldHeight) = (9, 9) :: (Int, Int)

fieldLength :: Float
fieldLength = cellSize * (fromIntegral fieldHeight)