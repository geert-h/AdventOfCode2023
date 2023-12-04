module Day3 where

day3 :: FilePath -> IO ()
day3 path = do
  file <- readFile path
  print file

data Schematic = Schematic
  { symbols :: [Symb],
    numbers :: [Numb]
  }

emptySchematic :: Schematic
emptySchematic = Schematic [] []

newtype Symb = Symb { sPos :: (Int, Int)}

data Numb = Numb
  { nPos :: (Int, Int),
    length :: Int,
    value :: Int
  }

parseSchematic :: String -> Schematic
parseSchematic s = fst $ foldr parseLine (emptySchematic, 0) (lines s)

parseLine :: String -> (Schematic, Int) -> (Schematic, Int)
parseLine s (schem, height) = (newSchem, height + 1)
  where
    newSchem :: Schematic
    newSchem = fst $ foldr parseChars (schem, 0, height) s
    
    parseChars :: Char -> (Schematic, Int, Int) -> (Schematic, Int, Int)
    parseChars c (schem', x, y) 
      | isDigit c = if y == lx && x == lx + llength + 1 then undefined
      where
        (lx,ly) = nPos lastNumber
        llength = length lastNumber
        lastNumber = last $ schem numbers 