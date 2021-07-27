module PE1 where

import Text.Printf


-- This function takes a Double and rounds it to 2 decimal places as requested in the PE --
getRounded :: Double -> Double
getRounded x = read s :: Double
               where s = printf "%.2f" x

-------------------------------------------------------------------------------------------
----------------------- DO NOT CHANGE ABOVE OR FUNCTION SIGNATURES-------------------------
------------- DUMMY IMPLEMENTATIONS ARE GIVEN TO PROVIDE A COMPILABLE TEMPLATE ------------
------------------- REPLACE THEM WITH YOUR COMPILABLE IMPLEMENTATIONS ---------------------
-------------------------------------------------------------------------------------------

convertTL :: Double -> String -> Double
convertTL money "USD" = getRounded usd
                        where usd = money / 8.18

convertTL money "EUR" = getRounded eur
                        where eur = money / 9.62

convertTL money "BTC" = getRounded btc
                        where btc = money / 473497.31

-------------------------------------------------------------------------------------------

countOnWatch :: [String] -> String -> Int -> Int
countOnWatch schedule employee days = length (findEmployee (take days schedule) employee)

findEmployee :: [String] -> String -> [String]
findEmployee schedule employee = [x | x <- schedule, x == employee]

-------------------------------------------------------------------------------------------

encrypt :: Int -> Int
encrypt x = (1000 * encrypt' (mod (div x 1000) 10)) + (100 * encrypt' (mod (div x 100) 10)) + (10 * encrypt' (mod (div x 10) 10)) + (encrypt' (mod x 10))

encrypt' :: Int -> Int
encrypt' digit
    | digit `mod` 3 == 0 = (digit - 1) `mod` 10
    | digit `mod` 4 == 0 = (digit * 2) `mod` 10
    | digit `mod` 5 == 0 = (digit + 3) `mod` 10
    | otherwise = (digit + 4) `mod` 10

-------------------------------------------------------------------------------------------

compoundInterests :: [(Double, Int)] -> [Double]
compoundInterests investments = [getRounded (calculateTotal initial_money time) | (initial_money, time) <- investments]

calculateTotal :: Double -> Int -> Double
calculateTotal money years = money * (1 + (interest_rate / 12))^(12*years)
                            where interest_rate = interestRate money years

interestRate :: Double -> Int -> Double
interestRate money years
    | money >= 10000 && years >= 2 = 0.115
    | money >= 10000 && years < 2 = 0.105
    | money < 10000 && years >= 2 = 0.095
    | money < 10000 && years < 2 = 0.090
