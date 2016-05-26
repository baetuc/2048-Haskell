import System.Random
import Data.List(elemIndices)

-- Synonim for the Int matrix necessary for holding the current state of the game
type GameCell = Int
type GameRow = [GameCell]
type GameMatrix = [GameRow]
nullCell = [0] -- for the Int Matrix
winValue = 2048
startMatrix = [[0,0,0,0], [0,2,0,0],[0,0,0,2], [4,0,0,0]]
listOfValues = [2,2,2,2,2,2,2,4]

---------------------------------- Utility functions ---------------------------

---------------------------------- The Transpose Function ----------------------

transpose :: GameMatrix -> GameMatrix
transpose ([] : _) = []
transpose matrix = (map head matrix) : transpose(map tail matrix)

----------------------------- The Right Rotate function ------------------------
-- The Right Rotate function stands in reversing the matrix and then applying the
-- transpose to the result

rotateRight :: GameMatrix -> GameMatrix
rotateRight = transpose.reverse

----------------------------- The Left Rotate function -------------------------
rotateLeft :: GameMatrix -> GameMatrix
rotateLeft ([] : _) = []
rotateLeft matrix = (map last matrix) : rotateLeft (map init matrix)

-------------------------------- Operations -----------------------------------
------------------------------------------------------------------------------

----We define the possible operations over the given Matrix, according to the user's input --------

-------------------------------- The Left move ------------------------------

-- Function that removes the empty spaces (defined by zeroes) in a specific line
trimSpaces :: GameRow -> GameRow
trimSpaces xs = filter (\x -> x /= 0) xs

-- Function that joins the "joinable" cells, using the 2048 algorithm
joinCellsLeft :: GameRow -> GameRow
joinCellsLeft (x:(y:xs))
          | x == y = (x+y) : (joinCellsLeft xs)
          | otherwise = x : (joinCellsLeft (y:xs))
joinCellsLeft xs = xs

-- Function that adds spaces to a row in order to achieve the initial length
addRowSpaces :: GameRow -> Int -> GameRow
addRowSpaces xs n
          | length xs < n = addRowSpaces (xs ++ nullCell) n
          | otherwise = xs

-- Function that encapsulates all the functions above, having as result the final
-- left move of the row
moveLineToLeft :: Int -> GameRow -> GameRow
moveLineToLeft n xs = addRowSpaces (joinCellsLeft (trimSpaces xs)) n

-- Function that applies the left move to the entire matrix
moveMatrixLeft :: GameMatrix -> GameMatrix
moveMatrixLeft matrix = map (moveLineToLeft (length matrix)) matrix

------------------------------- The Up move -----------------------------

-- Up move is defined over the Left move. All we have to do is to rotate the
-- matrix to the left, do the left move and then rotate the result to the right

moveMatrixUp :: GameMatrix -> GameMatrix
moveMatrixUp matrix = rotateRight $ moveMatrixLeft $ rotateLeft matrix

------------------------------- The Down move -----------------------------

-- Down move is defined over the Left move. All we have to do is to rotate the
-- matrix to the right, do the left move and then rotate the result to the left

moveMatrixDown :: GameMatrix -> GameMatrix
moveMatrixDown matrix = rotateLeft $ moveMatrixLeft $ rotateRight matrix

------------------------------- The Right move -----------------------------

-- Right move is defined over the Left move. All we have to do is to rotate the
-- matrix to the right twice, do the left move and then rotate the result to the
-- left twice

moveMatrixRight :: GameMatrix -> GameMatrix
moveMatrixRight matrix = rotateLeft $ rotateLeft $ moveMatrixLeft $ rotateRight $ rotateRight matrix


----------------------------- The Show Function on the Matrix ----------------

-- Function that returns the number of digits of the number given as parameter
numberDigits :: GameCell -> Int
numberDigits x
            | x < 10 = 1
            | otherwise = 1 + numberDigits (x `div` 10)

-- Function that returns the maximum digits necessary for representing any element
-- in a row
getMaximumDigits :: GameRow -> Int
getMaximumDigits [] = 0
getMaximumDigits (x:xs)
                    | currentDigits < recursiveDigits = recursiveDigits
                    | otherwise = currentDigits
                    where
                      currentDigits = numberDigits x
                      recursiveDigits = getMaximumDigits xs

-- Function that returns a list of numbers, each representing the maximum number
-- of digits necessary for representing a number in the specific column
getMaximumDigitsRow :: GameMatrix -> [Int]
getMaximumDigitsRow matrix = map getMaximumDigits (transpose matrix)

-- Function that prints a specified number of blank spaces
printPadding :: Int -> IO()
printPadding n
            | n > 0 = putChar ' ' >> printPadding (n - 1)
            | otherwise = putChar ' '

-- Function that prints a game cell, with padding in order to achieve a specified
-- number of characters
printCell :: GameCell -> Int -> IO()
printCell cell digits = printPadding (digits - numberDigits cell) >> putStr (show cell)

--  Function that prints a Matrix row
printRow :: GameRow -> [Int] -> IO()
printRow [] _ = putChar '\n'
printRow (x:xs) (y:ys) = (printCell x y) >> printRow xs ys

-- Function that prints a matrix, having as parameter the matrix and a list of
-- numbers representing the necessary digits for each column
printMatrixHavingDigitsColumn :: GameMatrix -> [Int] -> IO()
printMatrixHavingDigitsColumn [] _ = putChar '\n'
printMatrixHavingDigitsColumn (row:rows) digitsRow = printRow row digitsRow >> printMatrixHavingDigitsColumn rows digitsRow
-- Function that prints a Matrix
printMatrix :: GameMatrix -> IO()
printMatrix matrix = printMatrixHavingDigitsColumn matrix (getMaximumDigitsRow matrix)

------------------------ Function that verify that the game can continue -------------

-- Verifies wheter or not the current matrix will change if we do one of the defined moves
-- (Left, Right, Up, Down). If not, it means the game is OVER
canContinue :: GameMatrix -> Bool
canContinue mat
              | moveMatrixLeft mat /= mat = True
              | moveMatrixRight mat /= mat = True
              | moveMatrixUp mat /= mat = True
              | moveMatrixDown mat /= mat = True
              | otherwise = False

-- Function that verifies wheter or not the game is won, i.e. there exists a
-- cell with the winValue
isWon :: GameMatrix -> Bool
isWon mat = (sum . map (length . filter (== winValue))) mat > 0

--------------------- Random number introduction function ------------------------

-- Function that counts how many empty spots do we have in the GameMatrix
countEmptySpotsInMatrix :: GameMatrix -> Int
countEmptySpotsInMatrix = sum . map (length . filter (== 0))

-- Function that counts how many empty spots do we have in a GameRow
countEmptySpotsInRow :: GameRow -> Int
countEmptySpotsInRow = length . filter (==0)

-- Function that generates a random position between the empty ones
generateRandomPosition :: GameMatrix -> IO(Int)
generateRandomPosition mat = randomRIO (0, (countEmptySpotsInMatrix mat) - 1)
-- randomRIO is a function from System.Random package

-- Function that generates a random value from 2 and 4, with the probability of 1/8 to generate a 4
generateRandomValue :: IO(Int)
generateRandomValue = randomRIO (0,7) >>= \x -> return $ listOfValues !! x

-- Function that introduces a value at a specific index where value is 0 in GameRow
introduceValueInGameRow :: GameRow -> Int -> Int -> GameRow
introduceValueInGameRow row pos val
                            | chosenZeroIndex == 0 = val : (drop 1 row)
                            | otherwise = take chosenZeroIndex row ++ [val] ++ drop (chosenZeroIndex + 1) row
                            where
                              chosenZeroIndex = (elemIndices 0 row) !! pos


--Function that takes as parameters a GameMatrix and an index of a given position
--and introdues a number (2 or 4) on the empty spot with the given index
introduceValueAtPosition :: GameMatrix -> Int -> Int -> GameMatrix
introduceValueAtPosition [] _ _ = []
introduceValueAtPosition (x : xs) pos val
                                    | pos < emptySpots = ((introduceValueInGameRow x pos val) : xs)
                                    | otherwise = (x : (introduceValueAtPosition xs (pos - emptySpots) val))
                                      where
                                        emptySpots = countEmptySpotsInRow x

-- Function that introduces a random number (2 or 4) in an empty spot in the matrix
introduceRandom :: GameMatrix -> IO(GameMatrix)
introduceRandom mat = do
                        randPos <- generateRandomPosition mat
                        randVal <- generateRandomValue
                        return $ introduceValueAtPosition mat randPos randVal

------------------------------- Take the input from the user -----------------------------------

-- Function that returns the input from the user
takeInput :: IO(String)
takeInput = do
          putStr "Please introduce your next move (l/r/u/d): "
          c <- getLine
          if (c == "l") || (c == "r") || (c == "u") || (c == "d")
            then return c
            else putStrLn "Please introduce a valid move (l/r/u/d)" >> takeInput

-- Function that moves the matrix based on the user input (l/r/u/d)
moveMatrixBasedOnUserInput :: GameMatrix -> IO(GameMatrix)
moveMatrixBasedOnUserInput mat = do
                                  move <- takeInput
                                  case move of
                                    "l" -> return $ moveMatrixLeft mat
                                    "r" -> return $ moveMatrixRight mat
                                    "u" -> return $ moveMatrixUp mat
                                    "d" -> return $ moveMatrixDown mat

----------------------------------- Generate the game flow ----------------------------------------------

-- Function that defines the game flow
gameFlow :: GameMatrix -> IO()
gameFlow mat = do
                if isWon mat then putStrLn "Game is won!! Congratulations!"
                  else
                    if not (canContinue mat) then putStrLn "No moves possible. Game is lost!!"
                      else
                        do
                          printMatrix mat
                          matrix <- moveMatrixBasedOnUserInput mat
                          newMatrix <- introduceRandom matrix
                          gameFlow newMatrix


------------------------------------------  CREATE THE AI ---------------------------------------

main :: IO ()
main = gameFlow startMatrix
