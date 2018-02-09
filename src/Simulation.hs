module Simulation(simulateWorld,
                  defaultSpider) where
import           Data.Matrix(multStd, fromList, toList)
import           SimulationState
import Debug.NoTrace

data Action = TurnLeft | TurnRight | Move deriving(Show)
reach = 6
simSpeed = 10 -- Sims per second

type View = [Field]
view :: World -> Position -> View
view world (x, y, Top)
  = [getField world x' y' | y' <- [y..y+reach], x' <- [x-1..x+1]]
view world (x, y, ToLeft)
  = [getField world (x - x') y' | x' <- [1..reach], y' <- [y-1..y+1]]
view world (x, y, Bot)
  = [getField world x' (y - y') | y' <- [1..reach], x' <- [x-1..x+1]]
view world (x, y, ToRight)
  = [getField world x' y' | x' <- [x..x+reach], y' <- [y-1..y+1]]

-- Size of spider is 3 * 3 * reach, i.e.: 54
type Spider = [Double]

action world spider pos = let u0 = trace ("World and position are: " ++ show (world, pos)) map toInt (view world pos)
                              u = trace ("Build View" ++ show u0) fromList (3*reach) 1 u0
                              m = trace "Buils Network" $ fromList 3 (3*reach) spider
                              w = trace "Multiply" $ multStd m u --output/action neurons
                              in trace ("chooseFrom" ++ show w) $ chooseFrom (toList w)
                              where
                                chooseFrom [x,y,z]
                                   | x > y && x > z = TurnLeft
                                   | y > z = TurnRight
                                   | otherwise = Move

defaultSpider :: Spider
defaultSpider = [12,8,17,4,3,12,7,11,4,10,18,9,9,7,19,14,10,13,18, 10,15,20,20,2,3,18,17,1,1,19,6,1,10,16,4,5,2,15,7,1,13,18,5,20,2,13,5,12,13,15,4,14,13,12]

simulateWorld :: Spider -> StateWithTime -> StateWithTime
simulateWorld spider (StateWithTime t (SimState world pos))
  = let n = simSpeed * floor t :: Int
        newState = steps n world pos
        in StateWithTime (t - fromIntegral n) newState
    where steps 0 world pos = SimState world pos
          steps n world pos = let pos' = singleStep world spider pos in trace ("New position is: " ++ show pos') steps (n-1) world pos'

singleStep world spider pos = let ac = action world spider pos
                                  in trace ("Attempt action: " ++ show ac) change world ac pos

tryMove world pos1 pos2 = case uncurry (getField world) pos2 of
                               Empty -> pos2
                               Goal  -> pos2
                               Wall  -> pos1

change world Move (x, y, Top) = let (x', y') = tryMove world (x, y) (x, y+1)
                                    in (x', y', Top)
change world Move (x, y, ToLeft) = let (x', y') = tryMove world (x, y) (x-1, y)
                                       in (x', y', ToLeft)
change world Move (x, y, Bot) = let (x', y') = tryMove world (x, y) (x, y-1)
                                    in (x', y', Bot)
change world Move (x, y, ToRight) = let (x', y') = tryMove world (x, y) (x+1, y)
                                        in (x', y', ToRight)

change _ t (x, y, o) = trace ("Reorient to " ++ show (reorient t o)) (x, y, reorient t o)

reorient TurnLeft Top      = ToLeft
reorient TurnRight Top     = ToRight
reorient TurnLeft ToLeft   = Bot
reorient TurnRight ToLeft  = Top
reorient TurnLeft Bot      = ToRight
reorient TurnRight Bot     = ToLeft
reorient TurnLeft ToRight  = Top
reorient TurnRight ToRight = Bot
