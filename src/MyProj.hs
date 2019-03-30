--{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE RecordWildCards #-}

module MyProj
    ( runMyProj
    ) where

import System.IO
import Graphics.Gloss.Interface.Pure.Game
import Debug.Trace

import Types
import Backend
import Const
import MyPictures

--функции для рисования, работы с состоянием игры

changeView :: Picture -> Picture --как преобразование координат
changeView pic = (translate (-fieldLength - 200) (-fieldLength) (scale 2 2 pic))

drawGame :: GameState -> Picture
--одновременно с выбором уровня поле все равно выводим, чтобы после победы оно сразу не пропадало
drawGame GameState {inGame = False, gameTable = tab, gameOver = state} = changeView (pictures ( gameChoice ++ (showCells tab) ++ grid ++ (winLabel state) ))
drawGame GameState {gameTable = tab, markPos = (x, y), gameOver = state, haveError = err, errorPos = (xe, ye)} = 
  changeView (pictures ( (showCells tab) ++ grid ++ (mark x y) ++ (errorMark err xe ye) ++ (winLabel state) ++ guide))

handleEvent :: Event -> GameState -> GameState --определеяет движение "маркера" и смену значений
handleEvent (EventKey (Char '1') Down _ _) game@GameState {inGame = False, levels = tables} = game {inGame = True, gameOver = False, gameTable = (tables !! 0)}
handleEvent (EventKey (Char '2') Down _ _) game@GameState {inGame = False, levels = tables} = game {inGame = True, gameOver = False, gameTable = (tables !! 1)}
handleEvent (EventKey (Char '3') Down _ _) game@GameState {inGame = False, levels = tables} = game {inGame = True, gameOver = False, gameTable = (tables !! 2)}
handleEvent _ game@GameState {inGame = False} = game --при выборе игры доступны только цифры 1,2,3
handleEvent (EventKey (Char 'r') Down _ _) game@GameState {inGame = True} = game {inGame = False, gameOver = False, gameTable = Table {content = []}} --в меню
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
handleEvent _ g = g

update :: Float -> GameState -> GameState
update _ game@GameState {inGame = True, gameOver = True} = game {inGame = False} --разгадан судоку => переход в меню
update _ game@GameState {inGame = False} = game
update _ game@GameState {gameTable = tab, markPos = pos, haveError = err} | (err == False) && (isCorrect tab == False) = game {haveError = True, errorPos = pos}
                                                                          | (err == True) && (isCorrect tab == True) = game {haveError = False}
                                                                          | otherwise = game {gameOver = solved tab}

-- точка входа --
runMyProj :: IO ()
runMyProj = do
  putStrLn "[Loading config files...]"
  fileContent1 <- readFile configPath1 --не чистая функция
  fileContent2 <- readFile configPath2
  fileContent3 <- readFile configPath3
  case (checkValues (parseFile fileContent1)) && (checkValues (parseFile fileContent2)) && (checkValues (parseFile fileContent3)) of
    False -> putStrLn "[ERROR]: Incorrect int value in config" 
    True -> case (checkSize (parseFile fileContent1)) && (checkSize (parseFile fileContent2)) && (checkSize (parseFile fileContent3)) of
      False -> putStrLn "[ERROR]: Incorrect number of values in config"
      True -> let tab1 = parseFile fileContent1
                  tab2 = parseFile fileContent2
                  tab3 = parseFile fileContent3
              in do
                let initState = GameState {inGame = False, gameTable = Table {content = []}, markPos = (0, 0), currentPos = (7, 7)
                , gameOver = False, haveError = False, errorPos = (0,0), levels = tab1 : tab2 : tab3 : []}
                play display bgColor stepsPerSecond initState drawGame handleEvent update

