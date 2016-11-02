import Data.List
import Data.Char
import Data.Maybe

--Zack Mryyan and Wesley Adams

main = putStrLn $ show (lotusSolver [5,0,0,0,1,6,0,0,0,0,3,0,0,0,7,0,6,2,1,0,0,0,1,7,0,0,6,0,0,5,0,3,6,7,2,0,0,2,1,0,0,4,0,0,4,0,0,1,0])

--checks if the lotus is solved and returns the solved Lotus or an empty array 
lotusSolver:: [Int] -> [Int]
lotusSolver unsolved = if index == Nothing
                       then unsolved
                       else guessAnswer unsolved tryList (fromJust index)
   where index = elemIndex 0 unsolved
         tryList = possibleSolution unsolved (fromJust index) 1

--takes unsolved list and brute forces values
guessAnswer:: [Int] -> [Int] -> Int -> [Int]
guessAnswer _ [] _ = [] --when the list of possible solutions is empty, returns empty list 
guessAnswer unsolved (x:xs) n = if solution == [] 
                              then guessAnswer unsolved xs n --try the next value in the possible solution list
                              else solution 
   where solution = solve unsolved n x

--tries new  value and calls lotus solver and returns the solved lotus or empty list
solve:: [Int] -> Int -> Int -> [Int]
solve xs n try = if elemIndex 0 xs == Nothing --retuns solved lotus is solved 
                 then xs 
                 else lotusSolver (let (ys,_:zs) = splitAt n xs in ys ++ [try] ++ zs) --replace the number at n with a with a different possible solution

--returns a list of possible solutions at index "q"
possibleSolution:: [Int]-> Int -> Int -> [Int]
possibleSolution xs q location1 = 
                  if (location1 < 8) --checks which possible solutions work at this index
                  then if (location1 < 8)&&
				  ((xs !! ((clockwiseIndex !! location2) !! 0) /= location1)&&
				  (xs !! ((clockwiseIndex !! location2) !! 1) /= location1)&&
				  (xs !! ((clockwiseIndex !! location2) !! 2) /= location1)&&
				  (xs !! ((clockwiseIndex !! location2) !! 3) /= location1)&&
				  (xs !! ((clockwiseIndex !! location2) !! 4) /= location1)&&
				  (xs !! ((clockwiseIndex !! location2) !! 5) /= location1)&&
				  (xs !! ((clockwiseIndex !! location2) !! 6) /= location1)&&
				  (xs !! ((counterClockwiseIndex !! location3) !! 0) /= location1)&&
				  (xs !! ((counterClockwiseIndex !! location3) !! 1) /= location1)&&
				  (xs !! ((counterClockwiseIndex !! location3) !! 2) /= location1)&&
				  (xs !! ((counterClockwiseIndex !! location3) !! 3) /= location1)&&
				  (xs !! ((counterClockwiseIndex !! location3) !! 4) /= location1)&&
				  (xs !! ((counterClockwiseIndex !! location3) !! 5) /= location1)&&
				  (xs !! ((counterClockwiseIndex !! location3) !! 6) /= location1)&&
				  (xs !! ((circleIndex !! location4) !! 0) /= location1)&&
				  (xs !! ((circleIndex !! location4) !! 1) /= location1)&&
				  (xs !! ((circleIndex !! location4) !! 2) /= location1)&&
				  (xs !! ((circleIndex !! location4) !! 3) /= location1)&&
				  (xs !! ((circleIndex !! location4) !! 4) /= location1)&&
				  (xs !! ((circleIndex !! location4) !! 5) /= location1)&&
				  (xs !! ((circleIndex !! location4) !! 6) /= location1))
                       then possibleSolution xs q (location1 + 1) ++ [location1] -- recursively adds numbers til something works
                       else possibleSolution xs q (location1 + 1)
                  else [] --ends the recursion
   where
         location2 = findIndexClockwise q -- determines arcs to check
         location3 = findIndexCounterClockwise q
         location4 = checkCircle q -- determines ring to fix				 
				 
--list storing clockwise arcs indexes
clockwiseIndex = [[0,7,15,22,30,37,45],[1,8,16,23,31,38,46],[2,9,17,24,32,39,47],[3,10,18,25,33,40,48],[4,11,19,26,34,41,42],[5,12,20,27,28,35,43],[6,13,14,21,29,36,44]]::[[Int]]

--list storing counterclockwise arcs indexes
counterClockwiseIndex = [[0,13,20,26,33,39,46],[1,7,14,27,34,40,47],[2,8,15,21,28,41,48],[3,9,16,22,29,35,42],[4,10,17,23,30,36,43],[5,11,18,24,31,37,44],[6,12,19,25,32,38,45]]::[[Int]]

--list storing circle indexes
circleIndex = [[0,1,2,3,4,5,6],[7,8,9,10,11,12,13],[14,15,16,17,18,19,20],[21,22,23,24,25,26,27],[28,29,30,31,32,33,34],[35,36,37,38,39,40,41],[42,43,44,45,46,47,48]]::[[Int]]

--checks the index of the clockwise arcs that contain the checking values from Lotus
findIndexClockwise::Int -> Int
findIndexClockwise guessIndex
   | (guessIndex == 0)||(guessIndex == 7)||(guessIndex == 15)||(guessIndex == 22)||(guessIndex == 30)||(guessIndex == 37)||(guessIndex == 45) = 0
   | (guessIndex == 1)||(guessIndex == 8)||(guessIndex == 16)||(guessIndex == 23)||(guessIndex == 31)||(guessIndex == 38)||(guessIndex == 46) = 1
   | (guessIndex == 2)||(guessIndex == 9)||(guessIndex == 17)||(guessIndex == 24)||(guessIndex == 32)||(guessIndex == 39)||(guessIndex == 47) = 2
   | (guessIndex == 3)||(guessIndex == 10)||(guessIndex == 18)||(guessIndex == 25)||(guessIndex == 33)||(guessIndex == 40)||(guessIndex == 48) = 3
   | (guessIndex == 4)||(guessIndex == 11)||(guessIndex == 19)||(guessIndex == 26)||(guessIndex == 34)||(guessIndex == 41)||(guessIndex == 42) = 4
   | (guessIndex == 5)||(guessIndex == 12)||(guessIndex == 20)||(guessIndex == 27)||(guessIndex == 28)||(guessIndex == 35)||(guessIndex == 43) = 5
   | (guessIndex == 6)||(guessIndex == 13)||(guessIndex == 14)||(guessIndex == 21)||(guessIndex == 29)||(guessIndex == 36)||(guessIndex == 44) = 6
   | otherwise = 0

--checks the index of the counter clockwise arcs that contain the checking values from Lotus
findIndexCounterClockwise::Int -> Int
findIndexCounterClockwise guessIndex
   | (guessIndex == 0)||(guessIndex == 13)||(guessIndex == 20)||(guessIndex == 26)||(guessIndex == 33)||(guessIndex == 39)||(guessIndex == 46) = 0
   | (guessIndex == 1)||(guessIndex == 7)||(guessIndex == 14)||(guessIndex == 27)||(guessIndex == 34)||(guessIndex == 40)||(guessIndex == 47) = 1
   | (guessIndex == 2)||(guessIndex == 8)||(guessIndex == 15)||(guessIndex == 21)||(guessIndex == 28)||(guessIndex == 41)||(guessIndex == 48) = 2
   | (guessIndex == 3)||(guessIndex == 9)||(guessIndex == 16)||(guessIndex == 22)||(guessIndex == 29)||(guessIndex == 35)||(guessIndex == 42) = 3
   | (guessIndex == 4)||(guessIndex == 10)||(guessIndex == 17)||(guessIndex == 23)||(guessIndex == 30)||(guessIndex == 36)||(guessIndex == 43) = 4
   | (guessIndex == 5)||(guessIndex == 11)||(guessIndex == 18)||(guessIndex == 24)||(guessIndex == 31)||(guessIndex == 37)||(guessIndex == 44) = 5
   | (guessIndex == 6)||(guessIndex == 12)||(guessIndex == 19)||(guessIndex == 25)||(guessIndex == 32)||(guessIndex == 38)||(guessIndex == 45) = 6
   | otherwise = 0

--checks the index of the circle rings that contain the checking values from Lotus
checkCircle:: Int -> Int
checkCircle guessIndex 
   | (guessIndex == 0)||(guessIndex == 1)||(guessIndex == 2)||(guessIndex == 3)||(guessIndex == 4)||(guessIndex == 5)||(guessIndex == 6) = 0
   | (guessIndex == 7)||(guessIndex == 8)||(guessIndex == 9)||(guessIndex == 10)||(guessIndex == 11)||(guessIndex == 12)||(guessIndex == 13) = 1
   | (guessIndex == 14)||(guessIndex == 15)||(guessIndex == 16)||(guessIndex == 17)||(guessIndex == 18)||(guessIndex == 19)||(guessIndex == 20) = 2
   | (guessIndex == 21)||(guessIndex == 22)||(guessIndex == 23)||(guessIndex == 24)||(guessIndex == 25)||(guessIndex == 26)||(guessIndex == 27) = 3
   | (guessIndex == 28)||(guessIndex == 29)||(guessIndex == 30)||(guessIndex == 31)||(guessIndex == 32)||(guessIndex == 33)||(guessIndex == 34) = 4
   | (guessIndex == 35)||(guessIndex == 36)||(guessIndex == 37)||(guessIndex == 38)||(guessIndex == 39)||(guessIndex == 40)||(guessIndex == 41) = 5
   | (guessIndex == 42)||(guessIndex == 43)||(guessIndex == 44)||(guessIndex == 45)||(guessIndex == 46)||(guessIndex == 47)||(guessIndex == 48) = 6
   | otherwise = 0

				 