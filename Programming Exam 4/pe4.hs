module PE4 where

import Data.Maybe -- up to you if you want to use it or not

-- Generic DictTree definition with two type arguments
data DictTree k v = Node [(k, DictTree k v)] | Leaf v deriving Show

-- Lightweight Char wrapper as a 'safe' Digit type
newtype Digit = Digit Char deriving (Show, Eq, Ord) -- derive equality and comparison too!

-- Type aliases
type DigitTree = DictTree Digit String
type PhoneNumber = [Digit]


---------------------------------------------------------------------------------------------
------------------------- DO NOT CHANGE ABOVE OR FUNCTION SIGNATURES-------------------------
--------------- DUMMY IMPLEMENTATIONS ARE GIVEN TO PROVIDE A COMPILABLE TEMPLATE ------------
--------------------- REPLACE THEM WITH YOUR COMPILABLE IMPLEMENTATIONS ---------------------
---------------------------------------------------------------------------------------------


----------
-- Part I:
-- Some Maybe fun! Basic practice with an existing custom datatype.

digits = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']

-- toDigit: Safely convert a character to a digit
toDigit :: Char -> Maybe Digit
toDigit x = if elem x digits
                then Just (Digit x)
                else Nothing

-- toDigits: Safely convert a bunch of characters to a list of digits.
--           Particularly, an empty string should fail.
toDigits :: String -> Maybe PhoneNumber
toDigits [] = Nothing
toDigits str = if elem Nothing (convert str)
                    then Nothing
                    else Just [justDigit digit | digit <- (convert str), digit /= Nothing]

convert :: String -> [Maybe Digit]
convert str  = [toDigit char | char <- str]

justDigit :: Maybe Digit -> Digit
justDigit (Just digit) = digit


-----------
-- Part II:
-- Some phonebook business.

-- numContacts: Count the number of contacts in the phonebook...
numContacts :: DigitTree -> Int
numContacts tree = leafCount tree 0

leafCount :: DigitTree -> Int -> Int
leafCount (Leaf v) leafs = leafs + 1
leafCount (Node [(key, tree)]) leafs = leafCount tree leafs
leafCount (Node (first:rest)) leafs = leafCount (snd first) leafs + leafCount (Node rest) leafs

-- getContacts: Generate the contacts and their phone numbers in order given a tree. 
getContacts :: DigitTree -> [(PhoneNumber, String)]
getContacts tree = getContactsHelper tree [] []

getContactsHelper :: DigitTree -> PhoneNumber -> [(PhoneNumber, String)] -> [(PhoneNumber, String)]
getContactsHelper (Leaf v) phonenumber phonebook = phonebook ++ [(phonenumber, v)]
getContactsHelper (Node [(key, tree)]) phonenumber phonebook = getContactsHelper tree (phonenumber ++ [key]) phonebook
getContactsHelper (Node (first:rest)) phonenumber phonebook = (getContactsHelper (snd first) (phonenumber ++ [fst first]) phonebook) ++ (getContactsHelper (Node rest) phonenumber phonebook) 

-- autocomplete: Create an autocomplete list of contacts given a prefix
-- e.g. autocomplete "32" areaCodes -> 
--      [([Digit '2'], "Adana"), ([Digit '6'], "Hatay"), ([Digit '8'], "Osmaniye")]
autocomplete :: String -> DigitTree -> [(PhoneNumber, String)]
autocomplete [] _ = []
autocomplete str tree = dropPrev str (complete (findCompatible str (getPhonenumbers (getContacts tree)) 0 []) (getContacts tree) [])

getPhonenumbers :: [(PhoneNumber, String)] -> [String]
getPhonenumbers [] = []
getPhonenumbers (x:xs) = [[digitValue digit | digit <- fst x]] ++ getPhonenumbers xs

findCompatible :: String -> [String] -> Int -> [Int] -> [Int]
findCompatible _ [] counter index = index
findCompatible str (x:xs) counter index = if str == take (length str) x
                                                then findCompatible str xs (counter + 1) (index ++ [counter])
                                                else findCompatible str xs (counter + 1) index
                                                
complete :: [Int] -> [(PhoneNumber, String)] -> [(PhoneNumber, String)] -> [(PhoneNumber, String)]
complete [] phonebook found = found
complete (x:xs) phonebook found = complete xs phonebook (found ++ [phonebook !! x])

dropPrev :: String -> [(PhoneNumber, String)] -> [(PhoneNumber, String)]
dropPrev str [] = []
dropPrev str (x:xs) = [((drop (length str) (fst x)), snd x)] ++ dropPrev str xs
                        
digitValue :: Digit -> Char                                                        
digitValue (Digit k) = k

-----------
-- Example Trees
-- Two example trees to play around with, including THE exampleTree from the text. 
-- Feel free to delete these or change their names or whatever!

exampleTree :: DigitTree
exampleTree = Node [
    (Digit '1', Node [
        (Digit '3', Node [
            (Digit '7', Node [
                (Digit '8', Leaf "Jones")])]),
        (Digit '5', Leaf "Steele"),
        (Digit '9', Node [
            (Digit '1', Leaf "Marlow"),
            (Digit '2', Node [
                (Digit '3', Leaf "Stewart")])])]),
    (Digit '3', Leaf "Church"),
    (Digit '7', Node [
        (Digit '2', Leaf "Curry"),
        (Digit '7', Leaf "Hughes")])]

areaCodes :: DigitTree
areaCodes = Node [
    (Digit '3', Node [
        (Digit '1', Node [
            (Digit '2', Leaf "Ankara")]),
        (Digit '2', Node [
            (Digit '2', Leaf "Adana"),
            (Digit '6', Leaf "Hatay"),
            (Digit '8', Leaf "Osmaniye")])]),
    (Digit '4', Node [
        (Digit '6', Node [
            (Digit '6', Leaf "Artvin")])])]

