import Data.List

myLength :: [a] -> Int
myLength [] = 0
myLength (x:xs) = 1 + (myLength xs)

mean :: [Int] -> Maybe Double
mean [] = Nothing
mean xs = Just (total / count)
          where total = fromIntegral (sum xs)
                count = fromIntegral (length xs)

palindrome :: [a] -> [a]
palindrome xs = xs ++ (reverse xs)

isPalindrome :: Eq a => [a] -> Bool
isPalindrome [] = True
isPalindrome [_] = True
isPalindrome [x,y] = x == y
isPalindrome (x:xs) = (x == y) && (isPalindrome ys)
                      where y  = last xs
                            ys = init xs

sortLists :: [[a]] -> [[a]]
sortLists = sortOn length

joinLists :: a -> [[a]] -> [a]
joinLists _ [] = []
joinLists _ [x] = x
joinLists sep (x:xs) = x ++ [sep] ++ (joinLists sep xs)

data Tree a = Node a (Tree a) (Tree a)
            | Empty
              deriving (Show)

treeHeight :: Tree a -> Int
treeHeight Empty = 0
treeHeight (Node _ l r) = 1 + (maximum [(treeHeight l), (treeHeight r)])

data Direction = L | R | S
                 deriving (Show, Eq)

type Point = (Double, Double)
type Vector = (Double, Double)

magnitude :: Vector -> Double
magnitude (x, y) = sqrt ((x ** 2) + (y ** 2))

normalize :: Vector -> Vector
normalize v@(x, y) = let m = magnitude v in
                     ((x / m), (y / m))

sub :: Vector -> Vector -> Vector
sub (x1, y1) (x2, y2) = ((x1 - x2), (y1 - y2))

opp :: Point -> Double
opp = snd

adj :: Point -> Double
adj = fst

toAngle :: Vector -> Double
toAngle v = atan2 (opp v) (adj v)
-- toAngle v = let ang = atan2 (opp v) (adj v) in
--             if ang < 0 then ang + (2 * pi) else ang

dir :: Point -> Point -> Point -> Direction
dir a b c
    | cross > 0 = L
    | cross < 0 = R
    | otherwise = S
    where cross = (((fst b) - (fst a)) * ((snd c) - (snd a)))
                - (((snd b) - (snd a)) * ((fst c) - (fst a)))

-- TODO: Might be cool to do this with Maybe for lists less than three.
dirs :: [Point] -> [Direction]
dirs (x:y:z:xs) = (dir x y z):(dirs (y:z:xs))
dirs _ = []

grahamScan :: [Point] -> [Point]
grahamScan xs =
    case rightTurn of
        Nothing -> xs
        Just i  -> grahamScan (lowest:next:((take (i - 1) others) ++ (drop i others)))
    where byLowest          = sortOn snd xs
          lowest            = head byLowest
          angleFromLowest p = toAngle (sub p lowest)
          byAngle           = sortOn angleFromLowest (tail byLowest)
          next              = head byAngle
          others            = tail byAngle
          turns             = dirs (lowest:next:others)
          rightTurn         = elemIndex R turns

grahamScan2 :: [Point] -> [Point]
grahamScan2 xs = foldl checkTurn [next, lowest] others
                 where byLowest               = sortOn snd xs
                       lowest                 = head byLowest
                       angleFromLowest p      = toAngle (sub p lowest)
                       byAngle                = sortOn angleFromLowest (tail byLowest)
                       next                   = head byAngle
                       others                 = tail byAngle
                       checkTurn l@(y:z:xs) x = case (dir x y z) of
                                                    L -> x:z:xs
                                                    _ -> x:l

-- . . 5 . | . 3 . .
-- . . . . | . . . .
-- . . . . | . . . .
-- . . . . 4 . . . .
-- . . . . | . . . .
-- . 6 . . | . . 2 .
-- --------1--------
-- print (grahamScan  [(0, 0), (3, 1), (2, 6), (0, 3), ((-2), 6), ((-3), 1)])
