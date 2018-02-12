module Evolution where
import Simulation
import SimulationState
import System.Random
import Data.List
import Utilities


-- Utility Methods
update :: Int -> a -> [a] -> [a]
update _ _ [] = error "Index out of bound in update"
update 0 a (x:xs) = a:xs
update n a (x:xs) = x:update (n-1) a xs

randomDouble :: IO Double
randomDouble = let f g = fst $ randomR (0, 1) g
                   g = newStdGen
                   in fmap f g

randomInt :: Int -> Int -> IO Int
randomInt a b = let f g = fst $ randomR (a, b) g
                    in do g <- newStdGen
                          return (f g)

combineAt :: Int -> [a] -> [a] -> [a]
combineAt i l r = take i l ++ drop i r

-- Stuff
type Poputlation = [Spider]
randomPop :: Int -> IO Poputlation
randomPop n = let f g = take 54 $ randomRs (0, 1) g
                  in do g <- newStdGen
                        return (take n $ map f (iterate (snd . next) g))

pair (x:y:xs) = (x,y):pair xs
pair _ = []

evolutionStep :: Poputlation -> IO Poputlation
evolutionStep pop = let n = length pop
                        f (s1, s2) = replicate 4 (pairSpiders s1 s2)
                        children = mergeMonads $ concatMap f (pair pop)
                        in do nextGen <- children
                              return (take n $ reverse (sortOn fitness nextGen)) -- sort returns smallest first

evolutionSteps 0 pop = return pop
evolutionSteps n pop = evolutionStep pop >>= evolutionSteps (n-1)

-- TODO
mutateSpider :: Spider -> IO Spider
mutateSpider spider = do v <- randomDouble
                         i <- randomInt 0 53
                         return (update i v spider)

pairSpiders :: Spider -> Spider -> IO Spider
pairSpiders spider1 spider2 = do i <- randomInt 0 53
                                 return (combineAt i spider1 spider2)

fitness :: Spider -> Double
fitness spider = let endState = simulateSteps spider 50 initialState
                     gP = goalPosition endState
                     sP = spiderPosition endState
                     metric (x1, x2) (y1, y2) = (x1 - y1) ** 2 + (x2 - y2) ** 2
                     in metric gP sP
