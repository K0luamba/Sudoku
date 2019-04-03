module MyPictures where

import Graphics.Gloss.Interface.Pure.Game

import Types
import Const
import Backend (getValue, possible)

-- объекты --

cellToScreen :: (Int, Int) -> (Float, Float) --для перевода номера клетки в координаты
cellToScreen (x, y) = ((fromIntegral x) * cellSize, (fromIntegral y) * cellSize) 

gameChoice :: [Picture]
gameChoice = [translate (-120) fieldLength (label "Choice next puzzle from 1 to 3")]
  where 
    label s = (scale 0.2 0.2 (color orange (text s)))

grid :: [Picture] --черная сетка поверх отрисованных клеток
--проход по всем координатам клеток, для каждой контур квадрата (uncurry :: (a -> b -> c) -> (a,b) -> c)
grid = gridSm ++ gridV ++ gridH
  where 
    gridSm = [uncurry translate (cellToScreen (x, y)) (color black (rectangleWire cellSize cellSize)) | x <- [0 .. fieldWidth - 1], y <- [0 .. fieldHeight - 1] ]
    gridV = [translate (delta * k - cellSize/2) (lineLen/2 - cellSize/2) (color black (rectangleSolid 4 lineLen)) | k <- [0, 1, 2, 3] ]
    gridH = [translate (lineLen/2 - cellSize/2) (delta * k - cellSize/2) (color black (rectangleSolid lineLen 4)) | k <- [0, 1, 2, 3] ]
    lineLen = fieldLength + 2
    delta = cellSize * 3
    
mark :: Int -> Int -> [Picture] --вывод "курсора" - квадрата
mark x y = [uncurry translate (cellToScreen (x, y)) (color orange (rectangleWire cellSize cellSize))]

showCells :: Table -> [Picture] --непосредственно вывод таблицы
showCells tab = [uncurry translate (cellToScreen (x, y)) (drawCell tab x y) | x <- [0 .. fieldWidth - 1], y <- [0 .. fieldHeight - 1]]

drawCell :: Table -> Int -> Int -> Picture  --вывод клетки с позицией (x, y), тут отсчет иной: (номер столбца, номер строки снизу)
drawCell tab x y | cellValue == 0 = color white (rectangleSolid cellSize cellSize)
                 | otherwise = pictures [color white (rectangleSolid cellSize cellSize), label (show cellValue)]
  where --преобразование текстав картинку + масштабирование
    label n = translate (-5) (-7) (scale 0.15 0.15 (color black (text n)))
    cellValue = getValue tab x y

winLabel :: Bool -> [Picture]
winLabel state | state == True = [translate (-120) (fieldLength + 50) (label "Sudoku is solved, congratulations!")]
               | otherwise = [Blank]
  where 
    label s = (scale 0.2 0.2 (color orange (text s)))
               
errorMark :: Bool -> Int -> Int -> [Picture]
errorMark state x y | state == True = [uncurry translate (cellToScreen (x, y)) (color red (rectangleWire cellSize cellSize)), errorLabel]
                    | otherwise = [Blank]
  where
    errorLabel = translate (-40) fieldLength (scale 0.2 0.2 (color red (text "Firstly, fix this error!")))

guide :: [Picture]
guide = [hint1, hint2, hint3, hint4]
  where 
    hint1 = translate fieldLength (fieldLength - 50) (label "Use arrows to move input field.")
    hint2 = translate fieldLength (fieldLength - 100) (label "Press 0 to delete number in cell.")
    hint3 = translate fieldLength (fieldLength - 150) (label "Press R to return to the menu.")
    hint4 = translate fieldLength (fieldLength - 200) (label "Press H to show the hint.")
    label s = (scale 0.15 0.15 (color (makeColorI 10 20 100 255) (text s)))
    
hint :: Bool -> Table -> TablePos -> [Picture]
hint False _ _ = [Blank]
hint True tab pos | variants == [] = [translate (-10) (-50) (label1 "No variants of correct numbers, make some steps back.")]
                  | otherwise = [translate (-10) (-50) (label ("You can put here these numbers: " ++ (show variants) ++ "."))]
  where
    label s = (scale 0.15 0.15 (color (makeColorI 10 30 100 255) (text s)))
    label1 s = (scale 0.14 0.14 (color red (text s)))
    variants = possible tab pos