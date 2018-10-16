module Evolution where
import SimulationV2
import SimulationState
import System.Random
import Data.List
import Utilities
import Control.Monad.Random.Strict

-- Utility Methods
update :: Int -> a -> [a] -> [a]
update _ _ [] = error "Index out of bound in update"
update 0 a (x:xs) = a:xs
update n a (x:xs) = x:update (n-1) a xs

randomDouble :: RandomGen g => Double -> Double -> Rand g Double
randomDouble a b = getRandomR (a, b)

randomInt :: RandomGen g => Int -> Int -> Rand g Int
randomInt a b = getRandomR (a, b)

combineAt :: Int -> [a] -> [a] -> [a]
combineAt i l r = take i l ++ drop i r

-- Stuff
type Population = [Spider]
defaultPop :: Int -> IO Population
defaultPop n = let m = max chromosomes n
                   base = take m $ concatMap permutations [replicate l (-1) ++ replicate (chromosomes - l) 1 | l <- [1..chromosomes]]
                   rest = replicate (n-m) (replicate chromosomes 0)
                   pop = sortOn fitness (base ++ rest)
                   in return pop

randomPop :: Int -> IO Population
randomPop n = let f g = take chromosomes $ randomRs (-1, 1) g
                  in do g <- newStdGen
                        return (take n $ map f (iterate (snd . next) g))

pair (x:y:xs) = (x,y):pair xs
pair _ = []

evolutionStep :: RandomGen g => Population -> Rand g Population
evolutionStep pop = let n = length pop
                        pop' = take (n `div` 4) pop
                        f (s1, s2) = replicate 8 (pairSpiders s1 s2)
                        children = mergeMonads $ concatMap f (pair pop')
                        in do nextGen <- children
                              nextGenM <- mergeMonads (map mutateSpider nextGen)
                              return (take n $ sortOn fitness nextGenM) -- allow regression

evolutionSteps 0 pop = return pop
evolutionSteps n pop = evolutionStep pop >>= evolutionSteps (n-1)

-- Aufgaben:
delta = 0.1
grid = [s*i*delta | i <- [1..1/delta], s <- [-1, 1]]

mutateSpider :: RandomGen g => Spider -> Rand g Spider
mutateSpider spider = do j <- randomInt 0 (length grid - 1)
                         let v = grid!!j
                         i <- randomInt 0 (chromosomes - 1)
                         return (update i v spider)

pairSpiders :: RandomGen g => Spider -> Spider -> Rand g Spider
pairSpiders spider1 spider2 = do i <- randomInt 0 (chromosomes - 1)
                                 return (combineAt i spider1 spider2)

fitness1 :: Spider -> Double
fitness1 spider = let state1 = simulateSteps spider 10 initialState
                      state2 = simulateSteps spider 10 state1
                      state3 = simulateSteps spider 10 state2
                      sP1 = spiderPosition state1
                      sp2 = spiderPosition state2
                      sp3 = spiderPosition state3
                      in  fromIntegral (norm sP1 + norm sP1 * 2 + norm sP1 * 3)

fitness2 spider = let state1 = simulateSteps spider 30 initialState
                      in fromIntegral $ norm (spiderPosition state1)

fitness3 spider = let state = simulateSteps spider 30 initialState
                      in metric1 goalPosition (spiderPosition state)

fitness = fitness3

norm p = if p `elem` innerField then metric1 p goalPosition
         else 5 + metric1 p gatePosition
metric1 (x1, x2) (y1, y2) = abs (x1 - y1) + abs (x2 - y2)
gatePosition = (3, 7)
innerField = [(0, 5), (1, 7), (1, 6), (1, 5), (2, 7), (2, 6), (2, 5), (3, 7)]
