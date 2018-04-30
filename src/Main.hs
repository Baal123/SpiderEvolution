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
import           Control.Monad(when)


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
          winner <- startSim pop n
          putStrLn "Show Spider? (y)"
          a' <- noNewLine
          when (a' `elem` "yY") $ showSpider winner

startSim :: Population -> Int -> IO Spider
startSim pop n = do g <- newStdGen
                    let gen = evalRand (evolutionSteps n pop) g
                        winner = head gen
                    putStrLn "Calc Winner"
                    putStrLn $ "Fitness of winner is: " ++ show (fitness winner)
                    putStrLn "Continue with current Population? (y)"
                    a <- noNewLine -- for some reason newlines are thrown
                    putStrLn  "Answer is:"
                    print a
                    if a `elem` "yY"
                    then do putStrLn "How many Steps?"
                            getChar -- for some reason newlines are thrown
                            n' <- readInt
                            startSim gen n'
                    else do putStrLn "Return winner:"
                            return winner

isDigit x = x `elem` ['0','1','2','3','4','5','6','7','8','9']

noNewLine :: IO Char
noNewLine = do a <- getChar
               if a == '\n' then noNewLine
               else return a
