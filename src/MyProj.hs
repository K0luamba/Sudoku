--{-# OPTIONS_GHC -Wall #-}

module MyProj
    ( runMyProj
    ) where

import Data.List

type CurrentPos = (Int, Int) --номер Quad-а, позиция в Quad-е (1..9)

data Cell = Cell { value :: Maybe Int
                 } deriving Show

data Quad = Quad { cells :: [Cell] --9 ячеек
                 } deriving Show

data Table = Table { content :: [Quad] --9 квадратов
                   } deriving Show

createQuad :: [Int] -> Quad --для быстрого ввода программистом|для чтения из файла
createQuad numbers = Quad {cells = map (\x -> Cell {value = Just x}) numbers}

printQuad :: Quad -> IO () --для удобной проверки данных через консоль
printQuad (Quad {cells = l}) = print (map parseCell l) 
    where
      parseCell (Cell {value = Just x}) = x
      parseCell (Cell {value = Nothing}) = 0

printTable :: Table -> IO () --вывод каждого квадрата в одной строке
printTable (Table {content = []}) = putStr ""
printTable (Table {content = (quad : other)}) = do
  printQuad quad
  printTable (Table {content = other})

printFullTable :: Table -> IO () --адекватно выводит полностью заполненные таблицы
printFullTable tab = printLines (getLines tab)

printLines :: [[Int]] -> IO () 
printLines [] = putStr ""
printLines (line : other) = do 
  print line
  printLines other

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

getQuades :: Table -> [[Int]]
getQuades (Table {content = quads}) = map parseQuad quads
  where
    parseQuad (Quad {cells = []}) = []
    parseQuad (Quad {cells = (Cell (Just x) : other)}) = x : parseQuad (Quad {cells = other}) --все числа квадрата -> список
    parseQuad (Quad {cells = (Cell Nothing : other)}) = parseQuad (Quad {cells = other}) --незаполненные клетки просто пропускаем

getLines :: Table -> [[Int]]
getLines tab = transpose (transpose (getLines1 tab)) --так нужно для дебаггинга
  where
    getLines1 (Table {content = []}) = [[]]
    getLines1 (Table {content = (quad1:quad2:quad3:other)}) = quadsToLines quad1 quad2 quad3 ++ getLines (Table {content = other})

parseCells :: [Cell] -> [Int] --оставляет только числа, "пустышки" выкидывает
parseCells [] = []
parseCells (Cell Nothing : other) = parseCells other
parseCells (Cell (Just x) : other) = x : parseCells other

quadsToLines :: Quad -> Quad -> Quad -> [[Int]] --берет 3 квадрата одной линии, выдает 3 соответствущих строки в виде клеток
quadsToLines (Quad {cells = []}) _ _ = [[]] --когда прошли 3 раза
quadsToLines (Quad {cells = (x1:y1:z1:l1)}) (Quad {cells = (x2:y2:z2:l2)}) (Quad {cells = (x3:y3:z3:l3)}) = 
  [parseCells ((x1:y1:z1:[]) ++ (x2:y2:z2:[]) ++ (x3:y3:z3:[]))] ++ quadsToLines (Quad {cells = l1}) (Quad {cells = l2}) (Quad {cells = l3})

getColumns :: Table -> [[Int]]
getColumns tab = transpose (getLines tab)

isDifferent :: [Int] -> Bool --True, если все числа в списке разные
isDifferent [] = True
isDifferent (x : other)|x `elem` other = False --повтоение точно есть
                       | otherwise = isDifferent other

isCorrect :: Table -> Bool --проверяет, правильное ли заполнение у таблицы
isCorrect tab = correctColumns && (correctLines && correctQuades)
  where
    correctQuades = and (map isDifferent (getQuades tab))
    correctLines = and (map isDifferent (getLines tab))
    correctColumns = and (map isDifferent (getColumns tab))

isFull :: Table -> Bool --проверяет, заполнена ли таблица полностью
isFull table | sumLength == 81 = True
             | otherwise = False
  where
    sumLength = sum (map length (getQuades table))

--getTable :: FilePath -> Table --считывает числа, записанные по кваадратам из файла

--putValue :: Table -> currentPos -> Int -> Table --находит нужную клетку и добавляет/заменяет значение

--showTable :: Table -> Picture --рисует на экране введенные клетки

runMyProj :: IO ()
runMyProj = do
  putStrLn "[running MyProj]"
  print (isCorrect tableExample)
