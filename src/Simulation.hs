module Simulation(simulateWorld,
                  defaultSpider,
                  Spider,
                  simulateSteps) where
import           Data.Matrix     (fromList, multStd, toList)
import           Debug.NoTrace
import           SimulationState

data Action = TurnLeft | TurnRight | Move deriving(Show)
reach = 6

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
defaultSpider :: Spider
defaultSpider
  = [3.443963592762389e-2,0.6907643025251275,0.8297200881728026,0.9494911992637026,0.5909051346161961,0.8425734526058662,0.8366972387232299,0.44478564800016984,0.42618211390084326,0.6292167821622076,0.7271515547532703,0.3476520747563968,0.6660994689983872,0.4277448629147902,0.4520070073601449,0.2667591922508532,0.9080438468053574,0.869365246573231,0.6686071052409718,1.1183321512033162e-2,0.9761638974389767,0.6683129203298172,0.39698692845990824,0.6257730062136024,0.9056320349178099,0.39421210059635936,0.19702139302021449,0.17384679399508007,0.3446159247977868,0.9639774160767659,0.9953784876884751,0.7131910668658339,0.28292863511118593,0.2087154027799717,0.7453886943826094,0.8587795789616688,0.8668592736882477,8.235528423680505e-2,0.5284259754836653,0.8600628437746395,0.590025690961065,0.16454521445416337,0.12948317561474498,0.5343643610113483,0.4778844190165301,0.6822426479338617,0.9082716460082859,0.1853294581490048,0.19134013571607666,0.1642069006398721,0.5128031250434972,0.2437968819631562,0.24623244590764515,0.47440059027702486]



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

change _ t (x, y, o) = trace ("Reorient to " ++ show (reorient t o)) (x, y, reorient t o)

reorient TurnLeft Top      = ToLeft
reorient TurnRight Top     = ToRight
reorient TurnLeft ToLeft   = Bot
reorient TurnRight ToLeft  = Top
reorient TurnLeft Bot      = ToRight
reorient TurnRight Bot     = ToLeft
reorient TurnLeft ToRight  = Top
reorient TurnRight ToRight = Bot
