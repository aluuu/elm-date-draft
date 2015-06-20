module Draft.Date where

import Array


type alias Day = Int

type alias Month = Int

type alias Year = Int

-- field name is julianDay but actualy it is modified julian day,
-- see https://en.wikipedia.org/wiki/Julian_day for details
type alias Date = { julianDay: Int }

clip : comparable -> comparable -> comparable -> comparable
clip a b x =
  if | x < a -> a
     | x > b -> b
     | otherwise -> x


toOrdinalDate : Date -> (Year, Day)
toOrdinalDate {julianDay} =
  let
    a = julianDay + 678575
    quadcent = a // 146097
    b = a % 146097
    cent = min (b // 36524) 3
    c = b - (cent * 36524)
    quad = c // 1461
    d = c % 1461
    y = min (d // 365) 3
    year = quadcent * 400 + cent * 100 + quad * 4 + y + 1
    yearDay = d - (y * 365) + 1
  in
    (year, yearDay)


fromOrdinalDate : (Year, Int) -> Date
fromOrdinalDate (year, day) =
  let
    y = year - 1
    maxDays = if isLeap year then 366 else 365
    yearDay = clip 1 maxDays day
  in
   {julianDay = yearDay + (365 * y) + (y // 4) - (y // 100) + (y // 400) - 678576}


isLeap : Year -> Bool
isLeap year =
  ((year % 4) == 0) && ((year % 100) /= 0 || (year % 400) == 0)


toMonthDay : List Int -> Int -> (Month, Day)
toMonthDay (n::ns) yearDay =
  if yearDay > n then
    let
      (month, monthDay) = (toMonthDay ns (yearDay - n))
    in
     (month + 1, monthDay)
  else
     (1, yearDay)


daysInMonths : Year -> List Int
daysInMonths year =
  let
    isLeap' = isLeap year
  in
   [31, if isLeap' then 29 else 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]


dayOfYear : Date -> Day
dayOfYear date = let (year, yearDay) = toOrdinalDate date in yearDay


year : Date -> Year
year date = let (year, yearDay) = toOrdinalDate date in year


month : Date -> Month
month date =
  let
    (year', yearDay') = toOrdinalDate date
    (month, monthDay) = toMonthDay (daysInMonths year') yearDay'
  in
   month


day : Date -> Day
day date =
  let
    (year', yearDay') = toOrdinalDate date
    (month, monthDay) = toMonthDay (daysInMonths year') yearDay'
  in
   monthDay


toGregorian : Date -> (Year, Month, Day)
toGregorian date =
  let
    (year', yearDay') = toOrdinalDate date
    (month, monthDay) = toMonthDay (daysInMonths year') yearDay'
  in
   (year', month, monthDay)


monthLength : Year -> Month -> Int
monthLength year month =
  let
    monthLengths = Array.fromList (daysInMonths year)
    month' = clip 1 12 month
    monthLength = Array.get (month' - 1) monthLengths
  in
   case monthLength of
     Nothing -> 30
     Just x -> x


fromGregorianToOrdinal : (Year, Month, Day) -> (Year, Day)
fromGregorianToOrdinal (year, month, monthDay) =
  let
    month' = clip 1 12 month
    day' = clip 1 (monthLength year month') monthDay
    k = (case (month' <= 2, isLeap year) of
            (True, _) -> 0
            (False, True) ->  -1
            (_, _) -> -2)
    yearDay = ((367 * month' - 362) // 12) + k + day'
  in
    (year, yearDay)


fromGregorian : (Year, Month, Day) -> Date
fromGregorian gregorianDate =
   fromGregorianToOrdinal gregorianDate |> fromOrdinalDate