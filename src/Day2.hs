module Day2 (day2) where
import Data.Char (digitToInt, isDigit)
import Data.List.Split
import Debug.Trace (traceShowId)

data Game = Game
  { index :: Int,
    redCubes :: Int,
    greenCubes :: Int,
    blueCubes :: Int
  } deriving (Show)

data Color = Red | Green | Blue
  deriving (Eq)

day2 :: FilePath -> IO ()
day2 path = do
  file <- readFile path
  let fileLines = lines file
  let games = parseLines fileLines 
  putStrLn $  show (sumOfValidGameIds games) ++  " and " ++ show (sumOfPowOfGames games)

sumOfValidGameIds :: [Game] -> Int
sumOfValidGameIds = foldr gameIsPossible 0

powOfGame :: Game -> Int
powOfGame g = redCubes g * greenCubes g * blueCubes g

sumOfPowOfGames :: [Game] -> Int
sumOfPowOfGames = sum . map powOfGame

gameIsPossible :: Game -> Int -> Int
gameIsPossible game acc = if redCubes game < 13 && greenCubes game < 14 && blueCubes game < 15 then acc + index game else acc  

parseLines :: [String] -> [Game]
parseLines = map parseLine

parseLine :: String -> Game
parseLine s = Game (read indexString) r g b
  where
    line = splitOn ": " s
    indexString = dropWhile (not . isDigit) (head line)
    bodyString = last line
    subsets = "; " `splitOn` bodyString
    splittedSubsets = map (", " `splitOn`) subsets
    parsedSplittedSubsets = map parseItems splittedSubsets
    rgbs = map sumItems parsedSplittedSubsets
    (r,g,b) = maxSubsets rgbs

parseItem :: String -> (Int, Color)
parseItem s = (read numString, stringToColor colorString)
  where
    splittedString = splitOn " " s
    numString = head splittedString
    colorString = last splittedString

parseItems :: [String] -> [(Int, Color)]
parseItems = map parseItem

sumItems :: [(Int, Color)] -> (Int, Int, Int)
sumItems = foldr accumulate (0,0,0)
 where
  accumulate item (r,g,b) = case snd item of
    Red -> (r + fst item, g, b)
    Green -> (r, g + fst item, b)
    Blue -> (r, g, b + fst item)

maxSubsets :: [(Int, Int, Int)] -> (Int, Int, Int)
maxSubsets = foldr (\(r,g,b) (r',g',b') -> (max r r', max g g', max b b')) (0,0,0)

stringToColor :: String -> Color
stringToColor s = case s of
  "red" -> Red
  "green" -> Green
  "blue" -> Blue
  _ -> error "Invalid color"

