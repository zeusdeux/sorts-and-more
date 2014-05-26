qcksrt :: (Ord a) => [a] -> [a]
qcksrt [] = []
qcksrt (a:al) = 
        let smOrEq = [x | x <- al, x <= a]
            larger = [x | x <- al, x > a]
        in qcksrt smOrEq ++ [a] ++ qcksrt larger

-- Mergesort

-- split in half
-- m = length of input list / 2

-- recursive sorts
-- sort a[1..m] i.e., Left list, call mergesort on the list recursively
-- sort a[m+1..n] i.e., Right list call mergesort on the list recursively


-- merge sorted sub-arrays using temp array
-- b = copy of a[1..m]

-- while i <= m and j <= n,
--    a[k++] = (a[j] < b[i]) ? a[j++] : b[i++]
--    invariant: a[1..k] in final position
-- while i <= m,
--    a[k++] = b[i++]
--    invariant: a[1..k] in final position

-- split function returns two lists from one input list
split :: [a] -> ([a],[a])
split x =
        let m = div (length x) 2
            in splitAt m x

-- merge as per the merge algorithm
merge :: (Ord a) => [a] -> [a] -> [a]
merge x [] = x ++ []
merge [] y = y ++ []
merge (x:xs) (y:ys)
    | x <= y = x:merge xs (y:ys)
    | otherwise = y:merge (x:xs) ys

-- combination of split, mergesrt and merge
mergesrt :: (Ord a) => [a] -> [a]
mergesrt [] = []
mergesrt [x] = [x]
mergesrt x = 
        let (l,r) = split x -- split list into Left (l) list and Right (r) list
            in merge (mergesrt l) (mergesrt r) -- recursively sort left and right lists and then merge them

-- Insertion sort
insertionsrt :: (Ord a) => [a] -> [a]
insertionsrt [] = []
insertionsrt [x] = [x]
insertionsrt (x:xs) = insert x (insertionsrt xs)

insert :: (Ord a) => a -> [a] -> [a]
insert x [] = [x]
insert x (y:ys)
    | x < y = x:y:ys
    | otherwise = y:(insert x ys)

insertAlt x ys = 
    let lesser = [y | y <- ys, x > y]
        greater = [y | y <- ys, x <= y]
        in lesser ++ [x] ++ greater

-- selection sort
min' :: Ord a => [a] -> a
min' [x] = x
min' (x:y:xs)
    | x > y = min' (y:xs)
    | otherwise = min' (x:xs)

swap :: Eq a => a -> [a] -> [a]
swap x [] = [x]
swap a (x:ys) = a:[if y == a then x else y | y <- ys]

swapAlt :: Eq a => a -> [a] -> [a]
swapAlt x [] = [x]
swapAlt a (x:y:xs)
    | x == a = x:y:xs
    | y == a = swapAlt a (y:x:xs)
    | otherwise = swapAlt a (y:swapAlt a (x:xs))

selectionsrt :: (Eq a, Ord a) => [a] -> [a]
selectionsrt [] = []
selectionsrt [x] = [x]
selectionsrt (x:xs) =
    let min = min' (x:xs) --get the min element from unsorted list
        (_:swapped) = swapAlt min (x:xs) --swap the min element with 1st element in list
        in min:selectionsrt swapped --recursively sort the remaining unsorted list