import System.IO
import Control.Monad
import Text.Read
import Text.Printf

rawReadingsToVectorList rawText = map words (lines rawText)
main = do
        -- let day1Part1Answer = 1292 :: Int
        -- let day1Part2Answer = 1262 :: Int
        handle <- openFile "day2input.txt" ReadMode
        content <- hGetContents handle
        -- print content
        let readings = rawReadingsToVectorList content
        print readings