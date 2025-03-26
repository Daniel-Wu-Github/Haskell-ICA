{-
Enter your name: Daniel Wu
enter the date: 2/7/2025

fill in the functions below, there is no need to change main.
-}

import System.IO

-- Define the type for family data  (done for you)
type Family = (String, String, Int)

-- Read the file and parse it into a list of Family tuples  (done for you)
parseFamilyData :: String -> [Family]
parseFamilyData content =
    map parseLine (lines content)
    where
        parseLine line = let parts = split ',' line
                         in (trim (parts !! 0), trim (parts !! 1), read (trim (parts !! 2)) :: Int)

-- Utility function to split a string by a delimiter (Done for you -used to read in file)
split :: Char -> String -> [String]
split _ [] = []
split delim str =
    let (first, rest) = break (== delim) str
    in first : case rest of
        [] -> []
        (_:xs) -> split delim xs

-- Trim whitespace from a string (Done for you -used to read in file)
trim :: String -> String
trim = unwords . words

-- 1. Count the number of families
countFamilies :: [Family] -> Int
-- Write this function
countFamilies xs = length xs

-- 2. Compute the total number of family members
totalMembers :: [Family] -> Int
--write this function
totalMembers xs = sum [members | (_,_,members) <- xs] -- takes the third element of the tuple

-- 3. Find the family with the most members
largestFamily :: [Family] -> (String, Int)
--write this function
largestFamily xs = 
    let (name, _, members) = foldr1 (\a@(_, _, m1) b@(_, _, m2) -> if m1 > m2 then a else b) xs -- folds the list to find the largest family by comparing the number of members
    in (name, members)

-- 4. Filter families with more than a given number of members
filterLargeFamilies :: Int -> [Family] -> [Family]
--write this function
filterLargeFamilies n xs = filter (\(_, _, members) -> members > n) xs -- filters the list to find families with more than n members

-- 5. Extract and format addresses
formatAddresses :: [Family] -> [String]
--write this function
formatAddresses xs = [add | (_,add,_) <- xs]

-- Main function to read the file and execute the functions
main :: IO ()
main = do
    handle <- openFile "families.txt" ReadMode
    content <- hGetContents handle
    let familyData = parseFamilyData content

    putStrLn $ "Number of families: " ++ show (countFamilies familyData)
    putStrLn $ "Total number of people: " ++ show (totalMembers familyData)
    putStrLn $ "Largest family: " ++ show (largestFamily familyData)
    putStrLn $ "Families with more than 3 members: " ++ show (filterLargeFamilies 3 familyData)
    putStrLn $ "Addresses: " ++ show (formatAddresses familyData)

    hClose handle