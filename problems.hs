import Control.Exception
-- Problem 1
myLast :: [a] -> Maybe a
myLast l = case l of
    [] -> Nothing
    [x] -> Just x
    x : xs -> myLast xs

test1_1 :: Integer
test1_1 = assert (myLast [1, 2] == Just 2) 0

-- Problem 2
myButLast :: [a] -> Maybe a
myButLast l = case l of
    [x1, x2] -> Just x1
    x1 : xs -> myButLast xs
    _ -> Nothing

test2_1 :: Integer
test2_1 = assert (myButLast [1, 2] == Just 2) 0
test2_2 :: Integer
test2_2 = assert (myButLast [1, 2, 3, 4, 5] == Just 4) 0

-- Problem 3
elementAt :: [a] -> Int -> Maybe a
elementAt list index
    | null list = Nothing
    | index < 1 = Nothing
    | index == 1 = Just (head list)
    | otherwise = elementAt xs (index - 1)
        where _:xs = list

test3_1 :: Integer
test3_1 = assert (elementAt [1, 2, 3] 1 == Just 1) 0
test3_2 :: Integer
test3_2 = assert (elementAt [1, 2, 3] 2 == Just 2) 0

-- Problem 4
myLength :: [a] -> Integer
myLength = foldr ((+) . const 1) 0

test4_1 :: Integer
test4_1 = assert (myLength [1, 2, 3, 4, 5] == 5) 5

-- Problem 5
myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

test5_1 = assert (myReverse [1, 2, 3, 4, 5] == [5, 4, 3, 2, 1]) 0

-- Problem 6