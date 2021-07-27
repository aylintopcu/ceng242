module PE2 where

---------------------------------------------------------------------------------------------
------------------------- DO NOT CHANGE ABOVE OR FUNCTION SIGNATURES-------------------------
--------------- DUMMY IMPLEMENTATIONS ARE GIVEN TO PROVIDE A COMPILABLE TEMPLATE ------------
--------------------- REPLACE THEM WITH YOUR COMPILABLE IMPLEMENTATIONS ---------------------
---------------------------------------------------------------------------------------------

-- Note: undefined is a value that causes an error when evaluated. Replace it with
-- a viable definition! Name your arguments as you like by changing the holes: _

--------------------------
-- Part I: Time to inf up!

-- naturals: The infinite list of natural numbers. That's it!
naturals :: [Integer]
naturals = [0..]

-- interleave: Interleave two lists, cutting off on the shorter list.
interleave :: [a] -> [a] -> [a]
interleave [] _ = []
interleave _ [] = []
interleave (x:xs) (y:ys) = [x, y] ++ interleave xs ys

-- integers: The infinite list of integers. Ordered as [0, -1, 1, -2, 2, -3, 3, -4, 4...].
integers :: [Integer]
integers = drop 1 $ interleave (map (*(-1)) naturals) naturals

--------------------------------
-- Part II: SJSON Prettification

-- splitOn: Split string on first occurence of character.
splitOn :: Char -> String -> (String, String)
splitOn char str = splitOn' char str []

splitOn' :: Char -> String -> String -> (String, String)
splitOn' _ [] first = (first, "")
splitOn' char (x:rest) first = if x == char
                                    then (first, rest)
                                    else splitOn' char rest (first ++ [x])

-- tokenizeS: Transform an SJSON string into a list of tokens.
tokenizeS :: String -> [String]
tokenizeS sjson = tokenizeS' sjson []

tokenizeS' :: String -> [String] -> [String]
tokenizeS' [] done = done
tokenizeS' (x:xs) done
    | x == '{' || x == '}' || x == ':' || x == ',' = tokenizeS' xs (done ++ [[x]])
    | x == '\'' = tokenizeS' (snd (splitOn '\'' xs)) (done ++ [(fst (splitOn '\'' xs))])
    | otherwise = tokenizeS' xs done

-- prettifyS: Prettify SJSON, better tokenize first!
prettifyS :: String -> String
prettifyS sjson = parse (tokenizeS sjson) 0 []

parse :: [String] -> Int -> String -> String
parse [] level pretty = pretty
parse (x:xs) level pretty
    | x == "{" = parse xs (level + 4) (pretty ++ "{\n" ++ (take (level + 4) [' ', ' '..]))
    | x == "}" = parse xs (level - 4) (pretty ++ "\n" ++ (take (level - 4) [' ', ' '..]) ++ "}")
    | x == ":" = parse xs level (pretty ++ ": ")
    | x == "," = parse xs level (pretty ++ ",\n" ++ (take level [' ', ' '..]))
    | otherwise = parse xs level (pretty ++ "'" ++ x ++ "'")

-- Good luck to you, friend and colleague!

