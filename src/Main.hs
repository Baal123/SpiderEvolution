module Main(main) where
import           Data.Maybe
import           Graphics.Gloss
import           Graphics.Gloss.Juicy
import           Simulation
import           SimulationState
import           Utilities
import           Evolution
import           Draw
import           System.Random
import           Control.Monad.Random.Strict


readInt :: IO Int
readInt = do v <- getLine
             if not $ all isDigit v
             then do putStrLn "Not an Integer"
                     readInt
             else do putStrLn ""
                     return (read v)

main = do putStrLn "Enter Population Size"
          popSize <- readInt
          putStrLn "Enter Number of Evolution Steps"
          n <- readInt
          putStrLn "Random Starting Poputlation? (y)"
          a <- getChar
          pop <- if a `elem` "yY"
                 then do putStrLn "Use Random Population"
                         randomPop popSize
                 else do putStrLn "Use Default Poputlation"
                         defaultPop popSize
          g <- newStdGen
          let gen = evalRand (evolutionSteps n pop) g
              winner = head gen
              in do putStrLn "Show Winner"
                    print winner
                    showSpider winner


isDigit x = x `elem` ['0','1','2','3','4','5','6','7','8','9']
