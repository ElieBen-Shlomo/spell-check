deleteAtN :: Int -> [a] -> [a]
deleteAtN _ [] = []
deleteAtN i (a:as)
   | i == 0 = as
   | otherwise = a : deleteAtN (i-1) as

addAtN :: Int -> a -> [a] -> [a]
addAtN n element list = (take n list) ++ [element] ++ (takeLast (length list - n) list) where
    takeLast n = reverse . take n . reverse 

removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates = rdHelper [] where 
    rdHelper seen [] = seen
    rdHelper seen (x:xs)
        | x `elem` seen = rdHelper seen xs
        | otherwise = rdHelper (seen ++ [x]) xs

perturbations :: String -> [String]
perturbations word =  removeDuplicates $ concat [deletions, insertions, transpositions] where
    deletions = [deleteAtN i word | i <- [0..length word-1]]
    insertions = [addAtN i letter word | i <- [0..length word-1], letter <- letters] where
        letters = "abcdefghijklmnopqrstuvwxyz"
    transpositions = [transpose i j word |
         i <- [0..length word-1],
         j <- [0..length word-1],
         i /= j] where
            transpose :: Int -> Int -> [a] -> [a]
            transpose i j xs = zipWith (\x y -> 
                if x == i then xs !! j
                else if x == j then xs !! i
                else y) [0..] xs
    replacements = [replacement i letter word | i <- [0..length word-1], letter <- letters] where
        replacement i letter word = addAtN i letter (deleteAtN i word)
        letters = "abcdefghijklmnopqrstuvwxyz"

main:: IO()
main = do
    print(perturbations "xyzww")