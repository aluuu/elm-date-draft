module Draft.Date
  exposing
    ( Date(..)
    , fromJulianDay
    , year
    , month
    , day
    , toOrdinalDate
    , fromOrdinalDate
    , toGregorian
    , fromGregorian
    , addDays
    , addMonths
    , addYears
    , diffDays
    , dayOfYear
    )

{-| Date manipulation module

# Types
@docs Date

# Constructors

@docs fromJulianDay, fromOrdinalDate, fromGregorian

# Accessing and manipulation

@docs day, month, year, dayOfYear, addDays, addMonths, addYears, diffDays

# Conversations

@docs toOrdinalDate, toGregorian
-}

import Array
import Draft.Utils exposing (clip)


type alias Day =
  Int


type alias Month =
  Int


type alias Year =
  Int


type alias MJD =
  Int


{-| Type that symbolizes Modified Julian day
-}
type Date
  = Days MJD


{-| Returns year and year day for the given date.
-}
toOrdinalDate : Date -> ( Year, Day )
toOrdinalDate (Days julianDay) =
  let
    a =
      julianDay + 678575

    quadcent =
      a // 146097

    b =
      a % 146097

    cent =
      min (b // 36524) 3

    c =
      b - (cent * 36524)

    quad =
      c // 1461

    d =
      c % 1461

    y =
      min (d // 365) 3

    year =
      quadcent * 400 + cent * 100 + quad * 4 + y + 1

    yearDay =
      d - (y * 365) + 1
  in
    ( year, yearDay )


{-| Returns year and year day for the given date.
-}
fromJulianDay : Int -> Date
fromJulianDay =
  Days


{-| -}
fromOrdinalDate : ( Year, Int ) -> Date
fromOrdinalDate ( year, day ) =
  let
    y =
      year - 1

    maxDays =
      if isLeap year then
        366
      else
        365

    yearDay =
      clip 1 maxDays day

    julianDay =
      yearDay + (365 * y) + (y // 4) - (y // 100) + (y // 400) - 678576
  in
    Days julianDay


isLeap : Year -> Bool
isLeap year =
  ((year % 4) == 0) && ((year % 100) /= 0 || (year % 400) == 0)


toMonthDay : List Int -> Int -> ( Month, Day )
toMonthDay ns yearDay =
  case ns of
    n :: ns ->
      if yearDay > n then
        let
          ( month, monthDay ) =
            (toMonthDay ns (yearDay - n))
        in
          ( month + 1, monthDay )
      else
        ( 1, yearDay )

    [] ->
      ( 0, 0 )


daysInMonths : Year -> List Int
daysInMonths year =
  let
    isLeap' =
      isLeap year
  in
    [ 31
    , if isLeap' then
        29
      else
        28
    , 31
    , 30
    , 31
    , 30
    , 31
    , 31
    , 30
    , 31
    , 30
    , 31
    ]


{-| Returns year day of the given date.
-}
dayOfYear : Date -> Day
dayOfYear date =
  let
    ( year, yearDay ) =
      toOrdinalDate date
  in
    yearDay


{-| Returns year of the given date.
-}
year : Date -> Year
year date =
  let
    ( year, yearDay ) =
      toOrdinalDate date
  in
    year


{-| Returns month number of the given date.
-}
month : Date -> Month
month date =
  let
    ( year', yearDay' ) =
      toOrdinalDate date

    ( month, monthDay ) =
      toMonthDay (daysInMonths year') yearDay'
  in
    month


{-| Returns month day of the given date.
-}
day : Date -> Day
day date =
  let
    ( year', yearDay' ) =
      toOrdinalDate date

    ( month, monthDay ) =
      toMonthDay (daysInMonths year') yearDay'
  in
    monthDay


{-| Returns year, month and month day for the given date.
-}
toGregorian : Date -> ( Year, Month, Day )
toGregorian date =
  let
    ( year', yearDay' ) =
      toOrdinalDate date

    ( month, monthDay ) =
      toMonthDay (daysInMonths year') yearDay'
  in
    ( year', month, monthDay )


monthLength : Year -> Month -> Int
monthLength year month =
  let
    monthLengths =
      Array.fromList (daysInMonths year)

    month' =
      clip 1 12 month

    monthLength =
      Array.get (month' - 1) monthLengths
  in
    case monthLength of
      Nothing ->
        30

      Just x ->
        x


fromGregorianToOrdinal : ( Year, Month, Day ) -> ( Year, Day )
fromGregorianToOrdinal ( year, month, monthDay ) =
  let
    month' =
      clip 1 12 month

    day' =
      clip 1 (monthLength year month') monthDay

    k =
      (case ( month' <= 2, isLeap year ) of
        ( True, _ ) ->
          0

        ( False, True ) ->
          -1

        ( _, _ ) ->
          -2
      )

    yearDay =
      ((367 * month' - 362) // 12) + k + day'
  in
    ( year, yearDay )


{-| Constructs date value from tuple of year, month and month day.
-}
fromGregorian : ( Year, Month, Day ) -> Date
fromGregorian gregorianDate =
  fromGregorianToOrdinal gregorianDate |> fromOrdinalDate


{-| Adds given number of days to the date.
-}
addDays : Int -> Date -> Date
addDays daysCount (Days julianDay) =
  Days (julianDay + daysCount)


{-| Returns difference between to dates in days.
-}
diffDays : Date -> Date -> Int
diffDays (Days a) (Days b) =
  a - b


{-| Adds given number of months to the date.
-}
addMonths : Int -> Date -> Date
addMonths monthsCount date =
  let
    ( year, month, monthDay ) =
      toGregorian date

    rolloverMonths y m =
      ( y + ((m - 1) // 12), ((m - 1) % 12) + 1 )

    ( year', month' ) =
      rolloverMonths year (month + monthsCount)
  in
    fromGregorian ( year', month', monthDay )


{-| Adds given number of years to the date.
-}
addYears : Int -> Date -> Date
addYears yearsCount date =
  addMonths (yearsCount * 12) date
