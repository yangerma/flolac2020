-- This exercise covers the first 6 chapters of "Learn You a Haskell for Great Good!"

-- Chapter 1 - http://learnyouahaskell.com/introduction
-- Chapter 2 - http://learnyouahaskell.com/starting-out
-- Chapter 3 - http://learnyouahaskell.com/types-and-typeclasses
-- Chapter 4 - http://learnyouahaskell.com/syntax-in-functions
-- Chapter 5 - http://learnyouahaskell.com/recursion
-- Chapter 6 - http://learnyouahaskell.com/higher-order-functions

-- Download this file and then type ":l Chapter-1-6.hs" in GHCi to load this exercise
-- Some of the definitions are left "undefined", you should replace them with your answers.

-- Find the penultimate (second-to-last) element in list xs
penultimate xs = last (init xs)

-- Find the antepenultimate (third-to-last) element in list xs
antepenultimate xs = xs !! ((length xs) - 3)

-- Left shift list xs by 1
-- For example, "shiftLeft [1, 2, 3]" should return "[2, 3, 1]"
shiftLeft xs = (tail xs) ++ [(head xs)]

-- Left shift list xs by n
-- For example, "rotateLeft 2 [1, 2, 3]" should return "[3, 1, 2]"
rotateLeft 0 xs =  xs
rotateLeft n xs
    | n==0      = xs
    | otherwise = rotateLeft (n-1) (shiftLeft xs) 
-- Insert element x in list xs at index k
-- For example, "insertElem 100 3 [0,0,0,0,0]" should return [0,0,0,100,0,0]
insertElem x k xs = (take k xs) ++ [k] ++ (drop k xs)

-- Here we have a type for the 7 days of the week
-- Try typeclass functions like "show" or "maxBound" on them
data Day = Mon | Tue | Wed | Thu | Fri | Sat | Sun
         deriving (Eq, Ord, Show, Bounded, Enum)   

-- Note that if you try "succ Sun", you should get an error, because "succ" is not defined on "Sun"
-- Define "next", which is like "succ", but returns "Mon" on "next Sun"
next :: Day -> Day
next day
    | day==Sun  = Mon
    | otherwise = succ day

-- Return "True" on weekend
isWeekend :: Day -> Bool
isWeekend day = day >= Sat

data Task = Work | Shop | Play deriving (Eq, Show)

-- You are given a schedule, which is a list of pairs of Tasks and Days
schedule :: [(Task, Day)]
schedule = [(Shop, Fri), (Work, Tue), (Play, Mon), (Play, Fri)]

-- However, the schedule is a mess
-- Sort the schedule by Day, and return only a list of Tasks. 
-- If there are many Tasks in a Day, you should keep its original ordering
-- For example, "sortTask schedule" should return "[(Play, Mon), (Work, Tue), (Shop, Fri), (Play, Fri)]"
sortTask :: [(Task, Day)] -> [(Task, Day)]
sortTask sched
    | sched==[] = []
    | otherwise = sortedLeft ++ [piv] ++ sortedRight
    where piv   = head sched
          left  = [x | x <- (tail sched), (snd x) < (snd piv)]
          right = [x | x <- (tail sched), (snd x) >= (snd piv)]
          sortedLeft  = sortTask left 
          sortedRight = sortTask right

-- This function converts days to names, like "show", but a bit fancier
-- For example, "nameOfDay Mon" should return "Monday"
nameOfDay :: Day -> String
nameOfDay Mon = "Monday"
nameOfDay Tue = "Tuesday"
nameOfDay Wed = "Wednesday"
nameOfDay Thu = "Thrusday"
nameOfDay Fri = "Friday"
nameOfDay Sat = "Saturday"
nameOfDay Sun = "Sunday"

-- You shouldn't be working on the weekends
-- Return "False" if the Task is "Work" and the Day is "Sat" or "Sun"
labourCheck :: Task -> Day -> Bool
labourCheck task day = isWeekend day && (task == Work)

-- Raise x to the power y using recursion
-- For example, "power 3 4" should return "81"
power :: Int -> Int -> Int
power x 0 = 1
power x y = (power x (y-1)) * x
-- power x y = product (replicate y x)

-- Convert a list of booleans (big-endian) to a interger using recursion
-- For example, "convertBinaryDigit [True, False, False]" should return 4
convertBinaryDigit :: [Bool] -> Int
convertBinaryDigit bits
    | bits==[]  = 0
    | lsb       = head*2 + 1
    | otherwise = head*2
    where head = (convertBinaryDigit (init bits))
          lsb  = (last bits)
-- convertBinaryDigit bits = (convertBinaryDigit (init bits)) * 2
--     + (if (last bits) then 1 else 0)

-- Create a fibbonaci sequence of length N in reverse order
-- For example, "fib 5" should return "[3, 2, 1, 1, 0]"
fib :: Int -> [Int]
fib n
    | n<=2      = take n [1,0]
    | otherwise = nth : prev
    where prev = fib (n-1)
          nth  = sum (take 2 prev)

-- Determine whether a given list is a palindrome
-- For example, "palindrome []" or "palindrome [1, 3, 1]" should return "True"
palindrome :: Eq a => [a] -> Bool
palindrome xs
    | length xs < 2          = True
    | (head xs) /= (last xs) = False
    | otherwise              = palindrome (init (tail xs))

-- Map the first component of a pair with the given function
-- For example, "mapFirst (+3) (4, True)" should return "(7, True)"
mapFirst :: (a -> b) -> (a, c) -> (b, c)
mapFirst f pair = ((f (fst pair)), (snd pair))

-- Devise a function that has the following type
someFunction :: (a -> b -> c) -> (a -> b) -> a -> c
someFunction f g x = f x (g x)
