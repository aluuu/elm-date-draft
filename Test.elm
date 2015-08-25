import Check exposing (..)
import Check.Investigator exposing ( Investigator
                                   , investigator
                                   , rangeInt
                                   , float
                                   , int
                                   , char
                                   , shrink
                                   , random
                                   , lowerCaseChar
                                   , upperCaseChar
                                   )
import Check.Runner.Browser exposing (display)

import Draft.Date as D exposing (Date)
-- import Draft.Time as Time
-- import Draft.DateTime as DateTime
import Shrink
import String
import Random.Extra as Random

date : Investigator Date
date =
  let
    shrinker {julianDay} =
      Shrink.map (\d -> {julianDay=(abs d)}) (shrink int julianDay)

    generator =
      Random.map (\d -> {julianDay=(abs d)}) (random int)
  in
    investigator generator shrinker


dateSuite =
  suite "Draft.Date"
    [ claim "day number in ordinal date is between 1 and 366"
    `true`
      (\d -> let (y, yd) = D.toOrdinalDate d in 1 <= yd && yd <= 366)
    `for`
      date

    , claim "month day number is between 1 and 31"
    `true`
      (\d -> let md = D.day d in 1 <= md && md <= 31)
    `for`
        date

    , claim "month number is between 1 and 12"
    `true`
      (\d -> let m = D.month d in 1 <= m && m <= 12)
    `for`
        date

    , claim "year number should be greater or equal than 1858"
    `true`
      (\d -> let y = D.year d in 1858 <= y)
    `for`
      date ]

result =
    quickCheck dateSuite

main =
    display result