import System.IO
import Control.Monad
import Text.Read
import Text.Printf

rawReadingsToVectorList :: String -> SubVectorList
rawReadingsToVectorList rawText = map readingToVector readingList
        where readingList = map words (lines rawText)

getRaw :: String -> [[String]]
getRaw rawText = map words (lines rawText)

readingToVector :: [String] -> SubVector
readingToVector readingList = SubVector (head readingList) (read (last readingList))

type Direction = String
type Value = Int

data SubVector = SubVector Direction Value
type SubVectorList = [SubVector]

type HorizontalPosition = Int
type VerticalPosition = Int
type Aim = Maybe Int

data SubPosition = SubPosition HorizontalPosition VerticalPosition Aim
instance Show SubPosition where
        show (SubPosition horiz vert aim) = case aim of 
                Nothing -> mconcat ["Horizontal: ",show horiz," Vertical: ",show vert]
                Just aim -> mconcat ["Horizontal: ",show horiz," Vertical: ",show vert, " Aim: ", show aim]

positionValue :: SubPosition -> Int
positionValue (SubPosition horiz vert aim) = horiz * vert

changePosition :: SubPosition -> SubVector -> SubPosition
changePosition (SubPosition horiz vert aim) (SubVector dir val) = case aim of
        Nothing -> 
           case dir of
                "up" -> SubPosition horiz (vert - val) Nothing
                "down" -> SubPosition horiz (vert + val) Nothing
                "forward" -> SubPosition (horiz + val) vert Nothing
        Just aim -> 
           case dir of
                "up" -> SubPosition horiz vert (Just (aim - val))
                "down" -> SubPosition horiz vert (Just (aim + val))
                "forward" -> SubPosition (horiz + val) (vert + aim * val) (Just aim)

calculatePosition :: SubPosition -> SubVectorList -> SubPosition
calculatePosition startPos [] = startPos
calculatePosition startPos (x:xs) = calculatePosition (changePosition startPos x) xs

instance Show SubVector where
        show (SubVector dir val) = dir ++ " " ++ show val



main :: IO()
main = do
        let day2Part1Answer = 2150351 :: Int
        let day2Part2Answer = 1842742223 :: Int
        handle <- openFile "day2input.txt" ReadMode
        content <- hGetContents handle
        let vectors = rawReadingsToVectorList content
        let startPos = SubPosition 0 0 Nothing
        let endPos = calculatePosition startPos vectors 
        let endVal = positionValue endPos
        putStrLn $ "Part 1 submission:"
        print $ positionValue endPos
        if endVal == day2Part1Answer 
        then 
          print "Correct"
        else
          print "Incorrect"


        putStrLn $ "Part 2 submission:"
        let startPosAim = SubPosition 0 0 (Just 0)
        let endPosAim = calculatePosition startPosAim vectors 
        let endValAim = positionValue endPosAim
        print $ positionValue endPosAim
        if endValAim == day2Part2Answer 
        then 
          print "Correct"
        else
          print "Incorrect"