import System.IO
import Control.Monad
import Text.Read
import Text.Printf
-- import Data.List.Split (splitOn)

rawReadingsToIntList rawText = map (read :: String -> Int) (lines rawText) 

sumIncs :: [Int] -> Int
sumIncs (x:[]) = 0
sumIncs (x:xs) = if (head xs) > x
                 then 1 + sumIncs xs
                 else sumIncs xs

sumWindow :: [Int] -> Int
sumWindow readings = foldl (+) 0 readings

sumWindowIncs :: Int -> [Int] -> Int
sumWindowIncs windowSize readings = let currWindowSum = sumWindow (take windowSize readings)
                                        nextWindowSum = sumWindow (take windowSize (tail readings))
                                     in 
                                      if length nextWindowSum < windowSize
                                      then 0
                                      else
                                        if nextWindowSum > currWindowSum
                                         then 1 + sumWindowIncs windowSize (tail readings) 
                                         else sumWindowIncs windowSize (tail readings) 

main = do
        let day1Part1Answer = 1292 :: Int
        let day1Part2Answer = 1262 :: Int
        handle <- openFile "day1.txt" ReadMode
        content <- hGetContents handle
        let readings = rawReadingsToIntList content

        -- Part 1
        let incs = sumIncs readings
        -- print incs

        printf "Answer for Day 1 part 1: %d ~~ Calculated incs: %d\n" day1Part1Answer incs
        print $ incs == day1Part1Answer

        -- Part 2 
        let windowIncs = sumWindowIncs 3 readings
        print windowIncs
                
        printf "Answer for Day 1 part 2: %d ~~ Calculated incs: %d\n" day1Part2Answer windowIncs
        print $ windowIncs == day1Part2Answer