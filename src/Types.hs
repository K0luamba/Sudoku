module Types where

-- типы данных --

type TablePos  = (Int, Int) --номер Quad-а, позиция в Quad-е (1..9)

data Cell = Cell { value :: Maybe Int
                 } deriving Show

data Quad = Quad { cells :: [Cell] 
                 } deriving Show

data Table = Table { content :: [Quad] 
                   } deriving Show

data GameState = GameState { inGame :: Bool
                           , gameTable :: Table
                           , markPos :: (Int, Int) --frontend
                           , currentPos :: TablePos --backend
                           , gameOver :: Bool
                           , haveError :: Bool
                           , errorPos :: (Int, Int)
                           , levels :: [Table]
                           } deriving Show