module Sorting where

getNth :: [a] -> Int -> a 
getNth xs n
    | n < 0 = error "Negative index on list not allowed."
    | n > length xs - 1 = error "Index out of range." 
    | otherwise =
        let
            (_, zs) = splitAt n xs
        in
            head zs

removeNth :: [a] -> Int -> [a]
removeNth xs n
    | n < 0 = error "Negative index on list not allowed."
    | n > length xs - 1 = error "Index out of range."
    | otherwise =
        let
            (ys, zs) = splitAt n xs
        in
            ys ++ (tail zs)

insertAt :: [a] -> Int -> a -> [a]
insertAt xs n x
    | n < 0 = error "Negative index on list not allowed"
    | n > length xs = error "Index out of range"
    | n == length xs = xs ++ [x]
    | otherwise =
        let
            (ys, zs) = splitAt n xs
        in
            ys ++ [x] ++ zs

swap :: [a] -> Int -> Int -> [a]
swap xs n m
    | n == m = xs
    | otherwise =
        let
            xn = getNth xs n
            xm = getNth xs m
            xs' = insertAt (removeNth xs n) n xm
        in
            insertAt (removeNth xs' m) m xn

bubbleSortRound :: Ord a => [a] -> [a]
bubbleSortRound [] = []
bubbleSortRound [x] = [x]
bubbleSortRound [x, y]
    | x <= y = [x, y]
    | otherwise = [y, x]
bubbleSortRound (x:y:xs)
    | x <= y = x:(bubbleSortRound (y:xs))
    | otherwise = y:(bubbleSortRound (x:xs))

bubbleSort :: Ord a => [a] -> [a]
bubbleSort xs =
    let
        xs' = bubbleSortRound xs
        same = xs == xs'
    in
        if same
            then xs
            else bubbleSort xs'
         
