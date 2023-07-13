{--
You are given the following information, but you may prefer to do some research for yourself.
1 Jan 1900 was a Monday.
Thirty days has September,
April, June and November.
All the rest have thirty-one,
Saving February alone,
Which has twenty-eight, rain or shine.
And on leap years, twenty-nine.
A leap year occurs on any year evenly divisible by 4, but not on a century unless it is divisible by 400.
How many Sundays fell on the first of the month during the twentieth century (1 Jan 1901 to 31 Dec 2000)?
--}

-- days of week, days of month and months are 0 based in this representation
nextDay :: (Int,Int,Int,Int) -> (Int,Int,Int,Int) 
nextDay (dayOfWeek, day, month, year) = (dayOfWeek', day', month', year')
    where
        dayOfWeek' = mod (dayOfWeek + 1) 7
        day' = mod (day + 1) (numberOfDaysInMonth month)
        month' = if day' == 0 then mod (month + 1) 12 else month
        year' = if day' == 0 && month' == 0 then year + 1 else year
        numberOfDaysInMonth m = case m of 
            0 -> 31
            1 -> 28 + (if mod year 4 == 0 then 1 else 0)
            2 -> 31
            3 -> 30
            4 -> 31
            5 -> 30
            6 -> 31
            7 -> 31
            8 -> 30
            9 -> 31
            10 -> 30
            11 -> 31


main :: IO ()
main = print 
    $ length 
    $ filter (\(dayOfWeek, day, _, _) -> dayOfWeek == 6 && day == 0)
    $ takeWhile (\(_, day, month, year) -> (day, month, year) /= (30,11,2000)) 
    $ dropWhile (\(_, day, month, year) -> (day, month, year) /= (0,0,1901)) 
    $ iterate nextDay (0,0,0,1900)