--{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE RecordWildCards #-}

module MyProj
    ( runMyProj
    ) where

import Data.List
import System.IO
import Graphics.Gloss.Interface.Pure.Game
import Debug.Trace

-- типы данных --

type TablePos  = (Int, Int) --номер Quad-а, позиция в Quad-е (1..9)

data Cell = Cell { value :: Maybe Int
                 } deriving Show

data Quad = Quad { cells :: [Cell] 
                 } deriving Show

data Table = Table { content :: [Quad] 
                   } deriving Show

data GameState = GameState { gameTable :: Table
                           , markPos :: (Int, Int) --frontend
                           , currentPos :: TablePos --backend
                           , gameOver :: Bool
                           , haveError :: Bool
                           , errorPos :: (Int, Int)
                           } deriving Show

-- константы --

configPath :: FilePath
configPath = "config.txt"

tableExample :: Table 
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
          }

display :: Display --по сути определение размеров экрана
display = FullScreen

bgColor :: Color --фон
bgColor = greyN 0.9

stepsPerSecond :: Int
stepsPerSecond = 60

cellSize :: Float
cellSize = 24

fieldSize@(fieldWidth, fieldHeight) = (9, 9) :: (Int, Int)

-- функционал --

createQuad :: [Int] -> Quad --для быстрого ввода программистом|для чтения из файла
createQuad numbers = Quad {cells = map (\x -> Cell {value = Just x}) numbers}

printQuad :: Quad -> IO ()
printQuad (Quad {cells = l}) = print (map parseCell l) 
    where
      parseCell (Cell {value = Just x}) = x
      parseCell (Cell {value = Nothing}) = 0

printTableByQuades :: Table -> IO () --вывод каждого квадрата в одной строке
printTableByQuades (Table {content = []}) = putStr ""
printTableByQuades (Table {content = (quad : other)}) = do
  printQuad quad
  printTableByQuades (Table {content = other})

printTable :: Table -> IO () --нормальный построчный вывод
printTable tab = printLines (getLines tab)

printLines :: [[Int]] -> IO () 
printLines [] = putStr ""
printLines (line : other) = do 
  print line
  printLines other

getQuades :: Table -> [[Int]]
getQuades (Table {content = quads}) = map parseQuad quads
  where
    parseQuad (Quad {cells = []}) = []
    parseQuad (Quad {cells = (Cell (Just x) : other)}) = x : parseQuad (Quad {cells = other}) --все числа квадрата -> список
    parseQuad (Quad {cells = (Cell Nothing : other)}) = 0 : parseQuad (Quad {cells = other}) --незаполненные клетки gпомечаем нулями

getLines :: Table -> [[Int]]
getLines tab = transpose (transpose (getLines1 tab)) --так нужно для дебаггинга
  where
    getLines1 (Table {content = []}) = [[]]
    getLines1 (Table {content = (quad1:quad2:quad3:other)}) = quadsToLines quad1 quad2 quad3 ++ getLines (Table {content = other})

quadsToLines :: Quad -> Quad -> Quad -> [[Int]] --берет 3 квадрата одной линии, выдает 3 соответствущих строки в виде клеток
quadsToLines (Quad {cells = []}) _ _ = [[]] --когда прошли 3 раза
quadsToLines (Quad {cells = (x1:y1:z1:l1)}) (Quad {cells = (x2:y2:z2:l2)}) (Quad {cells = (x3:y3:z3:l3)}) = 
  [parseCells ((x1:y1:z1:[]) ++ (x2:y2:z2:[]) ++ (x3:y3:z3:[]))] ++ quadsToLines (Quad {cells = l1}) (Quad {cells = l2}) (Quad {cells = l3})

parseCells :: [Cell] -> [Int] --пустые помечает нулями
parseCells [] = []
parseCells (Cell Nothing : other) = 0 : parseCells other
parseCells (Cell (Just x) : other) = x : parseCells other

getColumns :: Table -> [[Int]]
getColumns tab = transpose (getLines tab)

isDifferent :: [Int] -> Bool --True, если все числа в списке разные
isDifferent [] = True
isDifferent (x : other) | x == 0 = isDifferent other --нули повторяться могут
                        | x `elem` other = False --повтоение точно есть
                        | otherwise = isDifferent other

isCorrect :: Table -> Bool --проверяет, правильное ли заполнение у таблицы
isCorrect tab = correctColumns && (correctLines && correctQuades)
  where
    correctQuades = and (map isDifferent (getQuades tab))
    correctLines = and (map isDifferent (getLines tab))
    correctColumns = and (map isDifferent (getColumns tab))

isFull :: Table -> Bool --проверяет, заполнена ли таблица полностью
isFull table | numOfFilled == 81 = True
             | otherwise = False
  where
    numOfFilled = sum (map notNull (getLines table))

notNull :: [Int] -> Int
notNull (x : other) | x == 0 = notNull other
                    | otherwise = 1 + notNull other
notNull [] = 0

parseFile :: String -> Table
parseFile str = (Table {content = map createQuad1 (readCells str)})
  where
    createQuad1 cellList = (Quad {cells = cellList})

readCells :: String -> [[Cell]] --содержание файла -> все клетки по квадратам
readCells = map (map readCell . words) . lines

readCell :: String -> Cell --одно поле таблицы -> одна ячейка
readCell "_" = Cell Nothing
readCell s = Cell (Just (read s))

checkValues :: Table -> Bool --проверяет, что все числа из диапазона [1..9]
checkValues tab = and (map isDigits (getQuades tab))
  where 
    isDigits (x : other) | (x >= 0) && (x <= 9) = isDigits other
                         | otherwise = False
    isDigits [] = True 

checkSize :: Table -> Bool --проверяет, что таблица получилась 9x9
checkSize tab = (and (map (\l -> length l == 9) (getQuades tab))) && (length (getQuades tab) == 9)

putValue :: Table -> TablePos  -> Int -> Table --находит нужную клетку и добавляет/заменяет/удаляет значение
putValue (Table {content = quads}) pos x = (Table {content = (putValueInQuades quads pos x)})

putValueInQuades :: [Quad] -> TablePos  -> Int -> [Quad]
--переходим на этап изменения текущего квадрата | идем дальше
putValueInQuades ( (Quad {cells = cellList}) : other) (quadNum, cellNum) x | quadNum == 1 = (Quad {cells = putValueInCells  cellList cellNum x}) : other 
                                                                           | otherwise = (Quad {cells = cellList}) : putValueInQuades other (quadNum - 1, cellNum) x
putValueInQuades [] _ _ = [] --если вдруг индекс квадрата вне [1..9], но сюда попадать не должны

putValueInCells :: [Cell] -> Int -> Int -> [Cell]
putValueInCells (first : other) pos x | pos == 1 = if x == 0 then (Cell {value = Nothing}) : other
                                                             else (Cell {value = Just x}) : other
                                      | otherwise = first : putValueInCells other (pos - 1) x
putValueInCells [] _ _ = [] 

cellToScreen :: (Int, Int) -> (Float, Float) --для перевода номера клетки в координаты
cellToScreen (x, y) = ((fromIntegral x) * cellSize, (fromIntegral y) * cellSize) 

drawGame :: GameState -> Picture
drawGame GameState {gameTable = tab, markPos = (x, y), gameOver = state, haveError = err, errorPos = (xe, ye)} = 
  pictures ( (showCells tab) ++ grid ++ (mark x y) ++ (errorMark err xe ye) ++ (winLabel state) )

grid :: [Picture] --черная сетка поверх отрисованных клеток
--проход по всем координатам клеток, для каждой контур квадрата (uncurry :: (a -> b -> c) -> (a,b) -> c)
grid = [uncurry translate (cellToScreen (x, y)) (color black (rectangleWire cellSize cellSize)) | x <- [0 .. fieldWidth - 1], y <- [0 .. fieldHeight - 1] ]

mark :: Int -> Int -> [Picture] --вывод "курсора" - квадрата
mark x y = [uncurry translate (cellToScreen (x, y)) (color orange (rectangleWire cellSize cellSize))]

showCells :: Table -> [Picture] --непосредственно вывод таблицы
showCells tab = [uncurry translate (cellToScreen (x, y)) (drawCell tab x y) | x <- [0 .. fieldWidth - 1], y <- [0 .. fieldHeight - 1]]

drawCell :: Table -> Int -> Int -> Picture  --вывод клетки с позицией (x, y), тут отсчет иной: (номер столбца, номер строки снизу)
drawCell tab x y | cellValue == 0 = color white (rectangleSolid cellSize cellSize)
                 | otherwise = pictures [color white (rectangleSolid cellSize cellSize), label (show cellValue)]
  where --преобразование текстав картинку + масштабирование
    label n = translate (-5) (-5) (scale 0.15 0.15 (color black (text n)))
    cellValue = getValue tab x y

getValue :: Table -> Int -> Int -> Int --значение в позиции (x,y) (см. drawCell) или 0
getValue tab x y = takeValue (reverse (getLines tab)) y x

takeValue :: [[Int]] -> Int -> Int -> Int --значение в строке <1ый арг> на позиции <2ой арг>
takeValue (first : other) line col | line == 0 = getValueInLine first col
                                   | otherwise = takeValue other (line - 1) col
takeValue [] _ _ = 0 --но вообще такого не должно быть

getValueInLine :: [Int] -> Int -> Int
getValueInLine (x : other) index | index == 0 = x
                                 | otherwise = getValueInLine other (index - 1)
getValueInLine [] _ = 0 

winLabel :: Bool -> [Picture]
winLabel state | state == True = [translate (-120) (cellSize * (fromIntegral fieldHeight)) (label "Sudoku is solved, congratulations!")]
               | otherwise = [Blank]
  where label s = (scale 0.2 0.2 (color orange (text s)))
               
errorMark :: Bool -> Int -> Int -> [Picture]
errorMark state x y | state == True = [uncurry translate (cellToScreen (x, y)) (color red (rectangleWire cellSize cellSize)), errorLabel]
                    | otherwise = [Blank]
  where
    errorLabel = translate (-40) (cellSize * (fromIntegral fieldHeight)) (scale 0.2 0.2 (color red (text "Firstly, fix this error!")))

handleEvent :: Event -> GameState -> GameState --определеяет движение "маркера" и смену значений
handleEvent _ game@GameState {gameOver = True} = game --блокировка действий после окончания игры
handleEvent (EventKey (SpecialKey KeyRight) Down _ _) game@GameState {markPos = (x, y), currentPos = (q, p), haveError = False} --при ошибке блокируем перемещение
    | x < 8 = game {markPos = (x + 1, y), currentPos = moveR (q, p)}
    | otherwise = game {markPos = (0, y), currentPos = moveR (q, p)}
handleEvent (EventKey (SpecialKey KeyLeft) Down _ _) game@GameState {markPos = (x, y), currentPos = (q, p), haveError = False}
    | x > 0 = game {markPos = (x - 1, y), currentPos = moveL (q, p)}
    | otherwise = game {markPos = (8, y), currentPos = moveL (q, p)}
handleEvent (EventKey (SpecialKey KeyUp) Down _ _) game@GameState {markPos = (x, y), currentPos = (q, p), haveError = False}
    | y < 8 = game {markPos = (x, y + 1), currentPos = moveU (q, p)}
    | otherwise = game {markPos = (x, 0), currentPos = moveU (q, p)}
handleEvent (EventKey (SpecialKey KeyDown) Down _ _) game@GameState {markPos = (x, y), currentPos = (q, p), haveError = False}
    | y > 0 = game {markPos = (x, y - 1), currentPos = moveD (q, p)}
    | otherwise = game {markPos = (x, 8), currentPos = moveD (q, p)}
handleEvent (EventKey (Char '0') Down _ _) game@GameState {gameTable = tab, currentPos = pos} = game {gameTable = putValue tab pos 0}
handleEvent (EventKey (Char '1') Down _ _) game@GameState {gameTable = tab, currentPos = pos} = game {gameTable = putValue tab pos 1}
handleEvent (EventKey (Char '2') Down _ _) game@GameState {gameTable = tab, currentPos = pos} = game {gameTable = putValue tab pos 2}
handleEvent (EventKey (Char '3') Down _ _) game@GameState {gameTable = tab, currentPos = pos} = game {gameTable = putValue tab pos 3}
handleEvent (EventKey (Char '4') Down _ _) game@GameState {gameTable = tab, currentPos = pos} = game {gameTable = putValue tab pos 4}
handleEvent (EventKey (Char '5') Down _ _) game@GameState {gameTable = tab, currentPos = pos} = game {gameTable = putValue tab pos 5}
handleEvent (EventKey (Char '6') Down _ _) game@GameState {gameTable = tab, currentPos = pos} = game {gameTable = putValue tab pos 6}
handleEvent (EventKey (Char '7') Down _ _) game@GameState {gameTable = tab, currentPos = pos} = game {gameTable = putValue tab pos 7}
handleEvent (EventKey (Char '8') Down _ _) game@GameState {gameTable = tab, currentPos = pos} = game {gameTable = putValue tab pos 8}
handleEvent (EventKey (Char '9') Down _ _) game@GameState {gameTable = tab, currentPos = pos} = game {gameTable = putValue tab pos 9}
handleEvent _ e = e

--4 функции для преобразования координат квадратов при движении "маркера"
moveR :: TablePos -> TablePos
moveR (q, p) | (p == 3) || (p == 6) || (p == 9) = (right q, p - 2) --переход между квадратами
             | otherwise = (q, p + 1) --сдвиг в рамках квадрата
  where
    right q | (q == 3) || (q == 6) || (q == 9) = q - 2
            | otherwise = q + 1

moveL :: TablePos -> TablePos
moveL (q, p) | (p == 1) || (p == 4) || (p == 7) = (left q, p + 2) 
             | otherwise = (q, p - 1)
  where
    left q | (q == 1) || (q == 4) || (q == 7) = q + 2
           | otherwise = q - 1

moveU :: TablePos -> TablePos
moveU (q, p) | (p == 1) || (p == 2) || (p == 3) = (up q, p + 6) 
             | otherwise = (q, p - 3)
  where
    up q | (q == 1) || (q == 2) || (q == 3) = q + 6
         | otherwise = q - 3

moveD :: TablePos -> TablePos
moveD (q, p) | (p == 7) || (p == 8) || (p == 9) = (down q, p - 6) --переход между квадратами
             | otherwise = (q, p + 3) --сдвиг в рамках квадрата
  where
    down q | (q == 7) || (q == 8) || (q == 9) = q - 6
           | otherwise = q + 3

update :: Float -> GameState -> GameState 
update _ game@GameState {gameTable = tab, markPos = pos, haveError = err} | (err == False) && (isCorrect tab == False) = game {haveError = True, errorPos = pos}
                                                                          | (err == True) && (isCorrect tab == True) = game {haveError = False}
                                                                          | otherwise = game {gameOver = solved tab}

solved :: Table -> Bool 
solved tab = (isFull tab) && (isCorrect tab)

-- точка входа --
runMyProj :: IO ()
runMyProj = do
  putStrLn "[running MyProj]"
  fileContent <- readFile configPath --не чистая функция
  case checkValues (parseFile fileContent) of
    False -> putStrLn "[ERROR]: Incorrect int value in config" 
    True -> case checkSize (parseFile fileContent) of
      False -> putStrLn "[ERROR]: Incorrect number of values in config"
      True -> let tab = parseFile fileContent
        in do
          printTable tab
          let initState = GameState {gameTable = tab, markPos = (0, 0), currentPos = (7, 7), gameOver = False, haveError = False, errorPos = (0,0)}
          play display bgColor stepsPerSecond initState drawGame handleEvent update

