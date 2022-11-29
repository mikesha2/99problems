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

test5_1 :: Integer
test5_1 = assert (myReverse [1, 2, 3, 4, 5] == [5, 4, 3, 2, 1]) 0

-- Problem 6
isPalindrome :: Eq a => [a] -> Bool
isPalindrome list = list == reverse list

test6_1 :: Integer
test6_1 = assert (isPalindrome [1, 2, 3, 4, 5, 4, 3, 2, 1]) 0
test6_2 :: Integer
test6_2 = assert (not $ isPalindrome [1, 2, 3, 4, 5]) 0

-- Problem 7
data NestedList a = Elem a | List [NestedList a]
    deriving (Eq, Show)
--flatten :: NestedList a -> [a]
flatten :: NestedList a -> [a]
flatten x = case x of
    List [] -> []
    Elem y -> [y]
    List (Elem a : xs) -> a : flatten (List xs)
    List (x : xs) -> flatten x ++ flatten (List xs)

test7_1 :: Integer
test7_1 = assert (null $ flatten (List [])) 0
test7_2 :: Integer
test7_2 = assert (flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]]) == [1, 2, 3, 4, 5]) 0

-- Problem 8
compress :: Eq a => [a] -> [a]
compress x = case x of
    [] -> []
    [x] -> [x]
    x1 : x2 : xs -> if x1 /= x2 then x1 : compress (x2 : xs) else compress (x2 : xs)

test8_1 :: Integer
test8_1 = assert (compress "aaaabccaadeeee" == "abcade") 0

-- Problem 9
pack :: Eq a => [a] -> [[a]]
packHelper :: Eq a => [a] -> [a] -> [[a]]
packHelper x acc = case x of
    [] -> [[]]
    [x] -> [x : acc]
    x1 : x2 : xs -> if x1 /= x2 then (x1 : acc) : packHelper (x2 : xs) [] else packHelper (x2 : xs) (x1 : acc)
pack x = packHelper x []

test9_1 :: Integer
test9_1 = assert (pack "aaaabccaadeeee" == ["aaaa", "b", "cc", "aa", "d", "eeee"]) 0

-- Problem 10
encodeHelper :: Eq a => [[a]] -> [(Int, a)]
encodeHelper x = case x of
    [] -> []
    (x : y) : xs -> (length (x : y), x) : encodeHelper xs
encode :: Eq a => [a] -> [(Int, a)]
encode = encodeHelper . pack

test10_1 :: Integer
test10_1 = assert (encode "aaaabccaadeeee" == [(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]) 0

-- Problem 11
data Repeat a = Single a | Multiple Int a
    deriving (Eq, Show)
encodeModified :: Eq a => [a] -> [Repeat a]
encodeModified l = map (\(n, x) -> if n == 1 then Single x else Multiple n x) $ encode l

test11_1 :: Integer
test11_1 = assert (encodeModified "aaaabccaadeeee" == [Multiple 4 'a', Single 'b', Multiple 2 'c', Multiple 2 'a',Single 'd', Multiple 4 'e']) 0

-- Problem 12
decodeModified :: [Repeat a] -> [a]
decodeModified x = case x of
    [] -> []
    (Single a : b) -> a : decodeModified b
    (Multiple n a : b) -> [a | i <- [1..n]] ++ decodeModified b

test12_1 :: Integer
test12_1 = assert (decodeModified
       [Multiple 4 'a', Single 'b', Multiple 2 'c',
        Multiple 2 'a', Single 'd', Multiple 4 'e'] == "aaaabccaadeeee") 0

-- Problem 13
encodeElement :: Int -> a -> Repeat a
encodeElement 1 y = Single y
encodeElement n y = Multiple n y

encodeDirect :: (Eq a) => [a] -> [Repeat a]
encodeDirect [] = []
encodeDirect (x:xs) = encodeDirectHelper 1 x xs
encodeDirectHelper :: Eq a => Int -> a -> [a] -> [Repeat a]
encodeDirectHelper n y [] = [encodeElement n y]
encodeDirectHelper n y (x:xs) | y == x    = encodeDirectHelper (n+1) y xs
                         | otherwise = encodeElement n y : encodeDirectHelper 1 x xs

test13_1 :: Integer
test13_1 = assert (encodeDirect "aaaabccaadeeee" == [Multiple 4 'a', Single 'b', Multiple 2 'c', Multiple 2 'a',Single 'd', Multiple 4 'e']) 0


-- Problem 14
dupli :: Foldable t => t b -> [b]
dupli = concatMap (\x -> [x, x])

test14_1 :: Integer
test14_1 = assert (dupli [1, 2, 3] == [1, 1, 2, 2, 3, 3]) 0

-- Problem 15
repli :: Foldable t => t b -> Int -> [b]
repli l n = concatMap (\x -> [x | i <- [1..n]]) l

test15_1 :: Integer
test15_1 = assert (repli [1, 2, 3] 3 == [1, 1, 1, 2, 2, 2, 3, 3, 3]) 0

-- Problem 16
dropEvery :: [a] -> Int -> [a]
dropEvery l n = map snd $ filter (\(i, _) -> mod i n /= 0) $ zip [1..length l] l

test16_1 :: Int
test16_1 = assert (dropEvery [1, 2, 3, 4, 5] 3 == [1, 2, 4, 5]) 0

-- Problem 17
split :: [a] -> Int -> ([a], [a])
splitHelper :: (Ord t, Num t) => [a] -> t -> [a] -> ([a], [a])
splitHelper l n acc | n <= 0 = (acc, l)
    | otherwise = let x:xs = l in
        splitHelper xs (n - 1) (x:acc)
split l n = let (l1, l2) = splitHelper l n [] in (reverse l1, l2)

test17_1 :: Int
test17_1 = assert (split [1, 2, 3, 4] 1 == ([1], [2, 3, 4])) 0

test17_2 :: Int
test17_2 = assert (split [1, 2, 3, 4] 3 == ([1, 2, 3], [4])) 0

-- Problem 18
slice :: [a] -> Int -> Int -> [a]
slice l a b = let (x, y) = split l (a-1) in
    let (x', y') = split y (b - a + 1) in x'

test18_1 :: Int
test18_1 = assert (slice ['a','b','c','d','e','f','g','h','i','k'] 3 7 == "cdefg") 0

-- Problem 19
rotate :: [a] -> Int -> [a]
rotate xs n = let l = n `mod` length xs in drop l xs ++ take l xs

test19_1 :: Int
test19_1 = assert (rotate ['a','b','c','d','e','f','g','h'] 3 == "defghabc") 0

test19_2 :: Int
test19_2 = assert (rotate ['a','b','c','d','e','f','g','h'] (-2) == "ghabcdef") 0
