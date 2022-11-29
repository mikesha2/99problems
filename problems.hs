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