module SimulationState(Orientation(..),
                       Position(..),
                       Field(..),
                       World(..),
                       SimState(..),
                       initialState,
                       getField,
                       toInt,
                       StateWithTime(..),
                       addTime) where
data Orientation = Top | ToLeft | Bot | ToRight deriving(Show)

type Position = (Int, Int, Orientation)

data Field = Empty | Wall | Goal deriving(Show)
toInt Empty = 0
toInt Wall = -1
toInt Goal = 1
fromInt 0 = Empty
fromInt (-1) = Wall
fromInt 1 = Goal

-- Rows are rows (horizontal)
data World = World [[Field]] Int Int deriving(Show)

getField (World field xSize ySize) x y
    | 0 <= x && x < xSize && 0 <= y && y < ySize = (field!!y)!!x
    | otherwise = Wall



data StateWithTime = StateWithTime Float SimState deriving(Show)
addTime t (StateWithTime t' g) = StateWithTime (t+t') g

data SimState = SimState World Position deriving(Show)

initialState = StateWithTime 0 $ SimState standardWorld startPosition
startPosition = (xSize - 7, ySize - 7, ToRight)
standardWorld = World field xSize ySize
field = [wall xSize,
         Wall:empty (xSize - 2) ++ [Wall],
         Wall:Empty:Wall:empty (xSize -4) ++ [Wall],
         Wall:Empty:Wall:empty (xSize -4) ++ [Wall],
         Wall:Empty:Wall:empty (xSize -4) ++ [Wall],
         Goal:Empty:Wall:empty (xSize -4) ++ [Wall],
         Wall:Empty:Wall:empty (xSize -4) ++ [Wall],
         Wall:Empty:Wall:empty (xSize -4) ++ [Wall],
         Wall:Empty:Wall:empty (xSize -4) ++ [Wall],
         Wall:empty (xSize - 2) ++ [Wall],
         wall xSize]

xSize = 16
ySize = length field
wall n = replicate n Wall
empty n = replicate n Empty
