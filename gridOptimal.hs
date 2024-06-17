import Data.Array
import Data.Maybe (Maybe(..),catMaybes, fromMaybe)
import Data.List (maximumBy,elemIndex)
import Data.Function (on)
import Text.Printf (printf)

--convert matrix to 1D array
arrayToList :: Array (Int, Int) Double -> [Double]
arrayToList arr =
    [arr ! (i, j) | i <- [minRow..maxRow], j <- [minCol..maxCol]]
    where
        ((minRow, minCol), (maxRow, maxCol)) = bounds arr

--print 1D array as grid,numbers are rounded to one decimal places 
formatResult :: [Double] -> Int -> String
formatResult [] _ = ""
formatResult (x:xs) size
    | length xs `mod` size == 0 = printf "%.1f\n" x ++ formatResult xs size
    | otherwise = printf "%.1f " x ++ formatResult xs size

--First update, groom positions
updateMatrixFirst :: Array (Int, Int) Double -> (Int, Int) -> Array (Int, Int) Double
updateMatrixFirst matrix (row, col) 
    | row == 0 && col == 1 = matrix // [((row, col), newValueGroom10)]
    | row == 0 && col == 3 = matrix // [((row, col), newValueGroom5)]
    | otherwise = matrix // [((row, col), ourPosValue)]
    where
        ourPosValue = matrix ! (row,col)       
        newValueGroom10 = 10 + (0.9*matrix ! (4,1))     
        newValueGroom5 =  5 + (0.9*matrix ! (2,3))     
    
--then update all without groom position
updateMatrix :: Array (Int, Int) Double -> (Int, Int) ->Int -> Array (Int, Int) Double
updateMatrix matrix (row, col) s
    | row == 0 && col == 1 =  matrix // [((row, col), ourPosValue)]
    | row == 0 && col == 3 = matrix // [((row, col), ourPosValue)]
    | otherwise = matrix // [((row, col), newValueStandard)]
    where
        upGrid = if row > 0 then Just (matrix ! (row - 1,col)) else Nothing             
        downGrid = if row < s - 1 then Just (matrix ! (row + 1,col)) else Nothing 
        leftGrid = if col > 0 then Just (matrix ! (row,col-1)) else Nothing           
        rightGrid = if col < s - 1 then Just (matrix ! (row,col+1)) else Nothing    
        allDirs = [upGrid, downGrid, leftGrid, rightGrid]
        maxValue = if null (catMaybes allDirs)
            then Nothing
            else Just (maximum $ catMaybes allDirs)                       
        maxValue1 =  maximum $ catMaybes allDirs
        ourPosValue = matrix ! (row,col)
        newValueStandard =1*(0+ 0.9*maxValue1)        


        
--iterations logic        
iterateUpdate :: Array (Int, Int) Double -> Int ->Int ->  Array (Int, Int) Double
iterateUpdate matrix 0 s = matrix
iterateUpdate matrix n s = iterateUpdate (foldl (\m (r, c) -> updateMatrix m (r, c) s) updatedMatrix indices) (n - 1) s
    where 
        updatedMatrix = foldl (\m (r, c) -> updateMatrixFirst m (r, c)) matrix indices 
        indices = [(r, c) | r <- [0..s-1], c <- [0..s-1]]


--main function/logic 
gridRL :: Int -> [Double]
gridRL size =
    let initialMatrix = array ((0, 0), (size - 1, size - 1)) [((i, j), 0.0) | i <- [0..size - 1], j <- [0..size - 1]]
        finalMatrix = iterateUpdate initialMatrix 15 size               --iteration number now is 15
    in arrayToList finalMatrix
    
     
main :: IO ()
main = do
    let size = 5
        result = gridRL size
    putStrLn $ formatResult result size    