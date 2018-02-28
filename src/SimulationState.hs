module SimulationState(Orientation(..),
                       Position(..),
                       Field(..),
                       World(..),
                       SimState(..),
                       initialState,
                       getField,
                       toInt,
                       StateWithTime(..),
                       addTime,
                       spiderPosition,
                       goalPosition) where
import Utilities
data Orientation = Top | ToLeft | Bot | ToRight deriving(Show, Eq, Ord)

type Position = (Int, Int, Orientation)

data Field = Empty | Wall | Goal deriving(Show, Eq, Ord)
toInt Empty = 0
toInt Wall = -1
toInt Goal = 1
fromInt 0 = Empty
fromInt (-1) = Wall
fromInt 1 = Goal

-- Rows are rows (horizontal)
data World = World [[Field]] Int Int deriving(Show, Eq, Ord)

getField (World field xSize ySize) x y
    | 0 <= x && x < xSize && 0 <= y && y < ySize = (field!!y)!!x
    | otherwise = Wall

data StateWithTime = StateWithTime Float SimState deriving(Show, Eq, Ord)
addTime t (StateWithTime t' g) = StateWithTime (t+t') g

data SimState = SimState World Position deriving(Show, Eq, Ord)
spiderPosition (SimState _ (x,y,_)) = (fromIntegral x, fromIntegral y)
goalPosition (SimState (World rows _ _) _ ) = let fields = concat $ mapWithIndex (\i row -> (mapWithIndex (\j cell -> ((i, j), cell)) row)) rows
                                                  (y, x) = fst . head $ filter (\x -> snd x == Goal) fields
                                                  in (fromIntegral x, fromIntegral y)

initialState = SimState standardWorld startPosition
startPosition = (xSize - 5, ySize - 3, ToRight)
standardWorld = World field xSize ySize
field = [wall xSize,
         Wall:empty (xSize - 2) ++ [Wall],
         Wall:Empty:Empty:empty (xSize -4) ++ [Wall],
         Wall:Empty:Empty:empty (xSize -4) ++ [Wall],
         Wall:Wall:Wall:Wall:empty (xSize -5) ++ [Wall],
         Goal:Empty:Empty:Wall:empty (xSize -5) ++ [Wall],
         Wall:Wall:Empty:Wall:empty (xSize -5) ++ [Wall],
         Wall:empty (xSize - 2) ++ [Wall],
         wall xSize]

xSize = 10
ySize = length field
wall n = replicate n Wall
empty n = replicate n Empty
