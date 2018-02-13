module Simulation(simulateWorld,
                  defaultSpider,
                  Spider,
                  simulateSteps,
                  chromosomes) where
import           Data.Matrix     (fromList, multStd, toList, fromLists, Matrix)
import           Debug.NoTrace
import           SimulationState

data Action = TurnLeft | TurnRight | Move | Wait deriving(Show)
reach = 5

type View = [Field]
view :: World -> Position -> View
view world (x, y, Bot)
  = [getField world x' y' | y' <- [y..y+reach], x' <- [x-2..x+2]]
view world (x, y, ToLeft)
  = [getField world (x - x') y' | x' <- [0..reach -1], y' <- [y-2..y+2]]
view world (x, y, Top)
  = [getField world x' (y - y') | y' <- [0..reach -1], x' <- [x-2..x+2]]
view world (x, y, ToRight)
  = [getField world x' y' | x' <- [x..x+reach], y' <- [y-2..y+2]]

-- Size of spider is 3 * 5 * reach
viewSize = 5 * reach
chromosomes = 2 * iLSize
type Spider = [Double]

defaultSpider :: Spider
defaultSpider = replicate chromosomes 0

inputWL view = map (\x -> if x == Wall then 1 else -1) view
inputGL view = map (\x -> if x == Goal then 1 else -1) view
inputEL view = map (\x -> if x == Empty then 1 else -1) view
intputLayer :: Num a => View -> Matrix a
intputLayer view = fromList iLSize 1 (inputWL view ++ inputEL view ++ inputGL view)
iLSize = 3*viewSize

-- weight matrices from spider
moveMatrix spider = fromList 1 iLSize $ take iLSize spider
turnLMatrix spider = fromList 1 iLSize $ drop iLSize spider

moveNeuron spider iL = neuron . head . toList $ moveMatrix spider `multStd` iL
turnLNeuron spider iL = neuron . head . toList $ turnLMatrix spider `multStd` iL
turnNeuron moveNeuron = (-1) * moveNeuron
turnLANeuron turnNeuron turnLNeuron = neuron $ turnNeuron + turnLNeuron -- weight vector = (1,1)
turnRANeuron turnNeuron turnLNeuron = neuron $ turnNeuron - turnLNeuron -- weight vector = (1,-1)

neuron x = if x > 0.25 then 1 else -1
fire = (==) 1

action world spider pos
  = let v = intputLayer (view world pos)
        mN = moveNeuron spider v
        tLN = turnLNeuron spider v
        tN = turnNeuron mN
        tLAN = turnLANeuron tN tLN
        tRAN = turnRANeuron tN tLN
        in chooseFrom (mN, tLAN, tRAN)
        where chooseFrom (x, y, z)
                | fire x = Move
                | fire y = TurnLeft
                | fire z = TurnRight
                | otherwise = Wait

simulateWorld :: Spider -> StateWithTime -> StateWithTime
simulateWorld spider (StateWithTime t state)
  = let n = floor t :: Int
        newState = simulateSteps spider n state
        in StateWithTime (t - fromIntegral n) newState

simulateSteps :: Spider -> Int -> SimState -> SimState
simulateSteps _ 0 state = state
simulateSteps spider n (SimState world pos) = let pos' = singleStep world spider pos in simulateSteps spider (n-1) (SimState world pos')

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

change _ Wait pos = pos
change _ t (x, y, o) = (x, y, reorient t o)

reorient TurnLeft Top      = ToLeft
reorient TurnRight Top     = ToRight
reorient TurnLeft ToLeft   = Bot
reorient TurnRight ToLeft  = Top
reorient TurnLeft Bot      = ToRight
reorient TurnRight Bot     = ToLeft
reorient TurnLeft ToRight  = Top
reorient TurnRight ToRight = Bot
