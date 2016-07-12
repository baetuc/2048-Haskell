import System.Random
import Data.List(elemIndices)
import Data.List(sort)
import Data.List(group)

-- Synonim for the Int matrix necessary for holding the current state of the game
type GameCell = Int
type GameRow = [GameCell]
type GameMatrix = [GameRow]
nullCell = [0] -- for the Int Matrix
winValue = 2048
startMatrix = [[0,0,0,0], [0,2,0,0],[0,0,0,2], [4,0,0,0]]
listOfValues = [2,2,2,2,2,2,2,4]
c1 = 5000
c2 = 0
c3 = 100
c4 = 100
treeDepth = 3
testCases = 30


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
                        if (countEmptySpotsInMatrix mat == 0) then return mat
                          else return $ introduceValueAtPosition mat randPos randVal

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


--------------------------------------------------------  Starting the AI ---------------------------------------------------------

--------------------------------------------- Expectimax algorithm --------------------------------------

-- Function that gets the specific function for the move specified by a character
getMoveForChar :: String -> GameMatrix -> GameMatrix
getMoveForChar move
                | move == "l" = moveMatrixLeft
                | move == "r" = moveMatrixRight
                | move == "u" = moveMatrixUp
                | otherwise = moveMatrixDown

-- Function that gets all the possible matrixes that the player could get to by specifying a move,
-- along with the move needed in order to get to that matrix from the current one
getDifferentMovedMatrix :: GameMatrix -> String -> [(GameMatrix, String)]
getDifferentMovedMatrix mat move = [((getMoveForChar move) mat, move)]

-- getDifferentMovedMatrix mat move      -- TO GET ONLY THE MOVES THAT change the state. To be tested
--                             | (getMoveForChar move) mat == mat = []
--                             | otherwise = [((getMoveForChar move) mat, move)]

-- Function that retrieves all possible matrixes that a user can reach from a move, different from the current matrix
getPossibleMatrixes :: GameMatrix -> [(GameMatrix, String)]
getPossibleMatrixes mat = getDifferentMovedMatrix mat "l" ++ getDifferentMovedMatrix mat "r" ++ getDifferentMovedMatrix mat "u"


-- Calculating the expectimax for a parent node (a player node)
parentExpectimax :: GameMatrix -> Int -> Int
parentExpectimax mat depth
                      | not (canContinue mat) = -1000000
                      | depth == 0 = heuristic mat
                      | otherwise = maximum $ map (childExpectimax (depth - 1)) $ map fst (getPossibleMatrixes mat)


-- Function that generates all the pairs (mat, r) from an initial matrix, where mat is a possible matrix to have
-- after the computer randomly introduces a value in the initial matrix, and r is 1 if the value is 4 and 9  if the value is 2
generateAllPossibilitiesWithProbability :: GameMatrix -> [(GameMatrix, Int)]
generateAllPossibilitiesWithProbability mat =
  [(introduceValueAtPosition mat i val, if val == 2 then 1 else 7) | i <- [0..(countEmptySpotsInMatrix mat - 1)], val <- [2,4]]

-- Calculating the expectimax for a child node (computer node)
childExpectimax ::  Int -> GameMatrix -> Int
childExpectimax depth mat
                    | depth == 0 = heuristic mat
                    | countEmptySpotsInMatrix mat == 0 = -1000000
                    | otherwise = sum (map (\(x, y) -> y * parentExpectimax x (depth - 1)) (generateAllPossibilitiesWithProbability mat)) `div` (10 * countEmptySpotsInMatrix mat)


-- Function that takes a decision based on current position
takeDecision :: GameMatrix -> IO(String)
takeDecision mat = return $ snd $ maximum $ map (\(x,y) -> (parentExpectimax x treeDepth, y)) (getPossibleMatrixes mat)

moveMatrixRandom :: GameMatrix -> IO(GameMatrix)
moveMatrixRandom mat
                | moveMatrixLeft mat /= mat = return $ moveMatrixLeft mat
                | moveMatrixRight mat /= mat = return $ moveMatrixRight mat
                | moveMatrixDown mat /= mat = return $ moveMatrixDown mat
                | otherwise = return $ moveMatrixUp mat

-- Function that moves the matrix based on the AI decision (l/r/u/d)
moveMatrixBasedOnAIDecision :: GameMatrix -> IO(GameMatrix)
moveMatrixBasedOnAIDecision mat = do
                                  move <- takeDecision mat
                                  if(countEmptySpotsInMatrix mat == 0) then moveMatrixRandom mat
                                    else
                                      case move of
                                        "l" -> return $ moveMatrixLeft mat
                                        "r" -> return $ moveMatrixRight mat
                                        "u" -> return $ moveMatrixUp mat
                                        "d" -> return $ moveMatrixDown mat

------------------------------------------ Heuristic ---------------------------------------------------
-- Function that calculates smoothness for a single row, i.e. differences between all adjacent cells
rowSmoothness :: GameRow -> Int
rowSmoothness (x : []) = 0
rowSmoothness (x : (y : xs)) = abs (x - y) + rowSmoothness (y : xs)

-- Function that calculates smoothness for all the matrix, i.e smoothness for each row plus smoothness for each column
-- (which is calculated as smoothness for each row of the turned matrix)
smoothness :: GameMatrix -> Int
smoothness mat = sum (map rowSmoothness mat) + sum (map rowSmoothness (rotateRight mat))

-- Function that calculates the score based on the distance from the border
borderDistanceScore :: (GameMatrix, Int, Int, Int, Int) -> Int
borderDistanceScore ([], curX, curY, maxX, maxY) = 0
borderDistanceScore (([] : yss), curX, curY, maxX, maxY) = borderDistanceScore (yss, 0, curY + 1, maxX, maxY)
borderDistanceScore (((x : xs) : yss), curX, curY, maxX, maxY) = (min curX (maxX - curX) + min curY (maxY - curY)) * x +
                                                                  borderDistanceScore ((xs : yss), curX + 1, curY, maxX, maxY)

matrixBorderDistanceScore :: GameMatrix -> Int
matrixBorderDistanceScore mat = borderDistanceScore (mat, 0, 0, length mat - 1, length mat - 1)

-- Function that returns the index of element in list, starting from a specifing index
getIndexOf :: GameRow -> GameCell -> Int -> Int
getIndexOf (x : xs) element index
                            | x == element = index
                            | otherwise = getIndexOf xs element (index - 1)

-- Function that returns 1 if the maximum element is in left cornet, otherwise returns 0
maxCellInLeftUpperCorner :: GameMatrix -> Int
maxCellInLeftUpperCorner mat
                          | getIndexOf (concat mat) (maximum (concat mat)) 0 == 0 = 1
                          | otherwise = 0

-- Calculating the heuristic score of a "leaf" node. It is defined by:
-- c1 * number of empty spots in the matrix - c2 * sum of modules of differences between all the adjacent cells (smoothness)
-- - c3 * value of cell * distance to the nearest border, where c1, c2 and c3 are constants, defined in the program header
heuristic :: GameMatrix -> Int
heuristic mat = c1 * (countEmptySpotsInMatrix mat) - c2 * (smoothness mat) - c3 * (matrixBorderDistanceScore mat) + c4 * (maxCellInLeftUpperCorner mat) * maximum(concat mat)


------------------------------------------  CREATE THE AI ---------------------------------------

----------------------------------- Generate the game flow ----------------------------------------------

-- Function that defines the game flow
gameFlow :: GameMatrix -> (GameMatrix -> IO(GameMatrix)) -> IO()
gameFlow mat inputSource = do
                            printMatrix mat
                            if isWon mat then putStrLn "Game is won!! Congratulations!"
                              else
                                if not (canContinue mat) then putStrLn "No moves possible. Game is lost!!"
                                  else
                                    do
                                      matrix <- inputSource mat
                                      newMatrix <- introduceRandom matrix
                                      gameFlow newMatrix inputSource

main :: IO ()
main = do
          putStr "Alegeti tipul de joc (A: automat / U : utilizator): "
          answer <- getLine
          case answer of
            "A" -> gameFlow startMatrix moveMatrixBasedOnAIDecision
            "U" -> gameFlow startMatrix moveMatrixBasedOnUserInput
            _ -> main

-------------------------------------- Functions used for statistics ------------------------------------


-- Function that returns the maximum tile achieved in a test case
testCase :: GameMatrix -> IO(Int)
testCase mat = do
                if not (canContinue mat) then return $ maximum (concat mat)
                  else
                    do
                      matrix <- moveMatrixBasedOnAIDecision mat
                      newMatrix <- introduceRandom matrix
                      testCase newMatrix

-- Function that runs a number of testes and collects all the results
groupTestCases :: Int -> IO([Int])
groupTestCases numberOfTestCases
                              | numberOfTestCases > 1 =
                                                    do
                                                      result <- testCase startMatrix
                                                      otherResults <- groupTestCases (numberOfTestCases - 1)
                                                      return (result : otherResults)
                              | otherwise = testCase startMatrix >>= \x -> return [x]

-- Function that returns statistics about all test cases
getStatistics :: [Int] -> IO([(Int, Int)])
getStatistics unorderedResults = return $ map (\x -> (x !! 0, length x)) $ group $ sort unorderedResults

-- Function that prints statistics
showStatistics :: [(Int, Int)] -> Int -> IO()
showStatistics [] tests = putStr ""
showStatistics ((x,y) : xs) tests = putStr ("S-a atins valoarea maxima ") >> putStr (show x) >> putStr " in " >>
                                      putStr (show ((fromIntegral y) * 100 / fromIntegral tests)) >> putStrLn "% dintre teste." >>
                                      showStatistics xs tests

showFinalResultForTestSuite :: [(Int, Int)] -> Int -> IO()
showFinalResultForTestSuite xs tests = putStr "Rezultate finale in urma a " >> putStr (show testCases) >> putStr " teste, in cadrul urmatorilor parametri: c1 = " >> putStr (show c1) >>
                                        putStr ", c2 = " >> putStr (show c2) >> putStr ", c3 = " >> putStr (show c3) >>
                                        putStr ", c4 = " >> putStr (show c4) >> putStr ", inaltimea = " >> putStr (show treeDepth) >>
                                        putStrLn ":" >> showStatistics xs tests

runTestsAndShowResults :: IO()
runTestsAndShowResults = groupTestCases testCases >>= \x -> getStatistics x >>= (\y -> showFinalResultForTestSuite y testCases)
