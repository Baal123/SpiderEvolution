module Brain where
import Data.Matrix (Matrix, fromList, fromLists, multStd2, toList, (!), ncols, mapPos)
import Utilities



type NeuronPosition = Int -- Position of neuron in Layer
type Neuron = (Int, NeuronPosition) -- (i,j) <-> Layer i, Neuron j
type Neurons = [Neuron]

type LayerActivity = Matrix Double -- Vector
type Weights = Matrix Double -- Weight Matrix
type Layer = (Weights, Neurons) -- Weights and the neurons they are connected to

type BrainDef = [Layer]


-- LayerActivity is saved in reverse Order
getInp :: Layer -> [LayerActivity] -> LayerActivity
getInp l inps = let ns = snd l
                    k = length inps
                    in vector (map (\(i, j) -> (inps!! (k - 1 - i))!(j + 1, 1)) ns)

-- LayerActivity is saved in reverse Order
feedInput :: BrainDef -> LayerActivity -> [LayerActivity]
feedInput brain input = feed brain [input]
                        where
                          feed [] outs = outs
                          feed (l:ls) outs = let inp = getInp l outs
                                                 out = fire $ fst l `multStd2` inp
                                                 in feed ls (out:outs) -- optimize this
--fire = id
fire :: (Fractional a, Ord a) => Matrix a -> Matrix a
fire = mapPos (\_ v -> if v > 0.1 then 1 else 0)

output brain input = head $ feedInput brain input

simpleFeedFoward :: [Weights] -> BrainDef
simpleFeedFoward ws = sff ws []
                      where
                        sff [] ls = reverse ls
                        sff (w:ws) ls = let inputDef = mapWithIndex (\j _ -> (0, j)) (replicate (ncols w) 0)
                                            l = (w, inputDef)
                                            in sff ws (l:ls)

-- Helper
vector ls = fromList (length ls) 1 ls
