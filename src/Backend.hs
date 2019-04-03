module Backend where

import Data.List

import Types

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

getValue :: Table -> Int -> Int -> Int --значение в позиции (x,y) (см. drawCell) или 0
getValue tab x y = takeValue (reverse (getLines tab)) y x

takeValue :: [[Int]] -> Int -> Int -> Int --значение в строке <2ой арг> на позиции <3ий арг>
takeValue (first : other) line col | line == 0 = getValueInLine first col
                                   | otherwise = takeValue other (line - 1) col
takeValue [] _ _ = 0 --но вообще такого не должно быть

getValueInLine :: [Int] -> Int -> Int
getValueInLine (x : other) index | index == 0 = x
                                 | otherwise = getValueInLine other (index - 1)
getValueInLine [] _ = 0 

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
           
solved :: Table -> Bool 
solved tab = (isFull tab) && (isCorrect tab)

--возвращает, какие значения могут стоять в данной клетке
possible :: Table -> TablePos -> [Int]
possible tab pos = possible1 tab pos [1 .. 9] 

--непосредственно осуществляет перебор оставшихся значений
possible1 :: Table -> TablePos -> [Int] -> [Int]
possible1 _ _ [] = []
possible1 tab pos (first : other) | canPut tab pos first == True = first : possible1 tab pos other
                                  | otherwise = possible1 tab pos other

--True, если вставить число на данную позицию таблицы
canPut :: Table -> TablePos -> Int -> Bool
canPut tab pos val = isCorrect (putValue tab pos val)





