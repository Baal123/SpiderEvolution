module Evolution where
import Simulation
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
defaultPop n = let m = min chromosomes n
                   base = take m [replicate l (-1) ++ replicate (chromosomes - l) 1 | l <- [1..chromosomes]]
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
                              return (take n $ sortOn fitness (nextGenM ++ pop)) -- do not regress

evolutionSteps 0 pop = return pop
evolutionSteps n pop = evolutionStep pop >>= evolutionSteps (n-1)

-- Aufgaben:
mutateSpider :: RandomGen g => Spider -> Rand g Spider
mutateSpider spider = do v <- randomDouble (-1) 1
                         i <- randomInt 0 (chromosomes - 1)
                         return (update i v spider)

pairSpiders :: RandomGen g => Spider -> Spider -> Rand g Spider
pairSpiders spider1 spider2 = do i <- randomInt 0 (chromosomes - 1)
                                 return (combineAt i spider1 spider2)

fitness :: Spider -> Double
fitness spider = let endState = simulateSteps spider 30 initialState
                     gP = goalPosition endState
                     sP = spiderPosition endState
                     metric (x1, x2) (y1, y2) = abs (x1 - y1) + abs (x2 - y2)
                     in metric gP sP
