module SimulationV2(simulateWorld,
                    Spider,
                    simulateSteps,
                    chromosomes) where
import           Data.Matrix     (fromList, multStd, toList, fromLists, Matrix, nrows, ncols)
import           Debug.NoTrace
import           SimulationState
import           Brain

-- View of the world
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
inputWL view = map (\x -> if x == Wall then 1 else 0) view
inputGL view = map (\x -> if x == Goal then 1 else 0) view
inputEL view = map (\x -> if x == Empty then 1 else 0) view

intputLayer :: Num a => View -> Matrix a
intputLayer view = fromList iLSize 1 (inputWL view ++ inputEL view ++ inputGL view)
iLSize = 3*viewSize

-- Spider Definition
type Spider = [Double]
type LayerDefinition = [Int]
layerDefinition = [6, 3]
chromosomes = f (iLSize:layerDefinition)
              where
                f (x:y:xs) = x*y + f (y:xs)
                f _ = 0

-- Brain
brain :: Spider -> LayerDefinition -> BrainDef
brain chromosomes layerDef
  = simpleFeedFoward $ brain0 chromosomes iLSize layerDef []
    where
      brain0 chromosomes previous [] def = reverse def
      brain0 chromosomes previous (s:ss) def = let (c, cs) = splitAt (s * previous) chromosomes
                                                   w = fromList s previous c
                                                   in brain0 cs s ss (w:def)

brainOfSpider chromosomes = brain chromosomes layerDefinition

brainOnView :: BrainDef -> View -> [Double]
brainOnView brain view = toList $ output brain (intputLayer view)


-- Brain activity to action
data Action = TurnLeft | TurnRight | Move | Wait deriving(Show)
action world brain pos = let v = view world pos
                             out = brainOnView brain v
                             in chooseFrom out
                             where chooseFrom [x, y, z]
                                      | x >= y && x >= z = Move --bias to move
                                      | y >= z = TurnLeft
                                      | otherwise = TurnRight

-- Simulation
simulateWorld :: Spider -> StateWithTime -> StateWithTime
simulateWorld spider (StateWithTime t state)
  = let n = floor t :: Int
        newState = simulateSteps spider n state
        in StateWithTime (t - fromIntegral n) newState

simulateSteps :: Spider -> Int -> SimState -> SimState
simulateSteps spider = simulateStepsB (brainOfSpider spider)

simulateStepsB _ 0 state = state
simulateStepsB brain n (SimState world pos)
  = let pos' = singleStep world brain pos
        in simulateStepsB brain (n-1) (SimState world pos')

singleStep world brain pos = let ac = action world brain pos
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
