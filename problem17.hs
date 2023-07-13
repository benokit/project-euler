-- If the numbers $1$ to $5$ are written out in words: one, two, three, four, five, then there are $3 + 3 + 5 + 4 + 4 = 19$ letters used in total.
-- If all the numbers from $1$ to $1000$ (one thousand) inclusive were written out in words, how many letters would be used? 
-- NOTE: Do not count spaces or hyphens. For example, $342$ (three hundred and forty-two) contains $23$ letters and $115$ (one hundred and fifteen) contains $20$ letters. The use of "and" when writing out numbers is in compliance with British usage.

toWords :: Int -> String
toWords 0 = ""
toWords 1 = "one"
toWords 2 = "two"
toWords 3 = "three"
toWords 4 = "four"
toWords 5 = "five"
toWords 6 = "six"
toWords 7 = "seven"
toWords 8 = "eight"
toWords 9 = "nine"
toWords 10 = "ten"
toWords 11 = "eleven"
toWords 12 = "twelve"
toWords 13 = "thirteen"
toWords 14 = "fourteen"
toWords 15 = "fifteen"
toWords 16 = "sixteen"
toWords 17 = "seventeen"
toWords 18 = "eighteen"
toWords 19 = "nineteen"
toWords 20 = "twenty"
toWords 30 = "thirty"
toWords 40 = "forty"
toWords 50 = "fifty"
toWords 60 = "sixty"
toWords 70 = "seventy"
toWords 80 = "eighty"
toWords 90 = "ninety"
toWords n 
    | div n 100 == 0 = let u = mod n 10 in toWords (n - u) ++ "-" ++ toWords u
    | div n 1000 == 0 = let d = toWords (mod n 100) in toWords (div n 100) ++ " hundred" ++ (if d == "" then d else " and " ++ d)  
    | div n 1000000 == 0 = let h = toWords (mod n 1000) in toWords (div n 1000) ++ " thousand" ++ (if h == "" then h else " " ++ h)

main :: IO ()
main = print $ sum $ map (length . filter (\c -> (c /= ' ') && (c /= '-')) . toWords) [1..1000]