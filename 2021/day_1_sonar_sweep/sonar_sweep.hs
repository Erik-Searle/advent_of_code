doubleMe x = x + x

doubleUs x y = doubleMe x + doubleMe y

isIncreasing list = not (length list == 1) && head list < head (tail list)

countIncreasingElements list = if not (null list)
                                     then if isIncreasing list
                                            then countIncreasingElements (tail list) + 1
                                            else countIncreasingElements (tail list)
                                     else 0

stringToInt :: String -> Integer
stringToInt string = read string

getSlidingWindowSums :: Num a => [a] -> Int -> [a]
getSlidingWindowSums list size = if size > length list
                                   then []
                                   else getNextWindowSum list size : getSlidingWindowSums (tail list) size

getNextWindowSum :: (Num a) => [a] -> Int -> a
getNextWindowSum list size = if size > 0 && length list >= size
                               then head list + getNextWindowSum (tail list) (size - 1)
                               else 0

main = do
  content <- getContents
  let list = lines content
      numList = map stringToInt list
  print (countIncreasingElements (getSlidingWindowSums numList 3))