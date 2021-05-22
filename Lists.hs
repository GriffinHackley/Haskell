module Lists where

countingNumbers :: [Int]
countingNumbers = [1,2..]

evenNumbers :: [Int]
evenNumbers = [2,4..]

primeNumbers :: [Int]
primeNumbers = [x | x <- [2..], (isPrime x)]

isPrime :: Integral a => a -> Bool
isPrime x = null [y | y <- [2..(x-1)], (mod x y) == 0]

merge :: (Ord a, Num a) => [a] -> [a] -> [a]
merge x [] = x
merge [] x = x
merge (x:x2) (y:y2) = 
  if x < y
    then x:(merge x2 (y:y2))
    else y:(merge (x:x2) y2)

wrap :: Int -> [a] -> [a]
wrap k [] = []
wrap k xs = snd(splitAt k xs)++fst(splitAt k xs)

slice :: (Int, Int) -> [a] -> [a]
slice (start, stop) numbers = snd(splitAt (start - 1) (fst(splitAt (stop) numbers)))

subLists :: [a] -> [[a]]
subLists [] = []
subLists (n) = (subLists (fst(splitAt ((length n)-1) n)))++[n]

countElements :: Foldable t => [t a] -> Int
countElements [] = 0
countElements (h:list) = ((length h) + (countElements list))

sortSubLists :: (Ord a, Foldable t, Num a) => [t a] -> [t a]
sortSubLists [h] = [h]
sortSubLists [h,h2] = 
  if (sum h) < (sum h2)
    then [h,h2]
    else [h2,h]
sortSubLists (h:h2:list) =
  if (sum h) < (sum h2)
    then h:sortSubLists(h2:list)
    else h2:sortSubLists(h:list)

listApply :: (Foldable t, Num a1) => (a2 -> a1 -> a1) -> [t a2] -> [a1]
listApply f [] = []
listApply f (h:list) = (foldr f 0 h):(listApply f list)

composeList :: [t -> t] -> t -> t
composeList [] x = x
composeList (h:list) x = (composeList list (h x)) 