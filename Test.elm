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
                                   , upperCaseChar )

import Draft.Date as Date exposing (Date(..), fromJulianDay)
import Draft.Time as Time exposing (Time(Milliseconds), fromMilliseconds)
import Shrink
import String
import Random.Extra as Random
import Check.Runner.IO exposing (display)

import IO.IO exposing (..)
import IO.Runner exposing (Request, Response)
import IO.Runner as IO

date : Investigator Date
date =
  let
    shrinker (Days julianDay) =
      Shrink.map (\d -> fromJulianDay (abs d)) (shrink int julianDay)

    generator = Random.map (\d -> fromJulianDay (abs d)) (random int)
  in
    investigator generator shrinker

time : Investigator Time
time =
  let
    shrinker (Milliseconds ms) =
      Shrink.map (\ms -> fromMilliseconds (abs ms)) (shrink int ms)

    generator = Random.map (\ms -> fromMilliseconds (abs ms)) (random int)
  in
    investigator generator shrinker

dateSuite =
  suite "Draft.Date"
    [ claim "Day number in ordinal date is between 1 and 366"
    `true`
      (\d -> let (y, yd) = Date.toOrdinalDate d in 1 <= yd && yd <= 366)
    `for`
      date

    , claim "Month day number is between 1 and 31"
    `true`
      (\d -> let md = Date.day d in 1 <= md && md <= 31)
    `for`
        date

    , claim "Month number is between 1 and 12"
    `true`
      (\d -> let m = Date.month d in 1 <= m && m <= 12)
    `for`
        date

    , claim "Year number should be greater or equal than 1858"
    `true`
      (\d -> let y = Date.year d in 1858 <= y)
    `for`
      date ]


timeSuite =
  suite "Draft.Time"
    [ claim "Hours number is in range from 0 to 23"
    `true`
      (\t -> let h = Time.hours t in 0 <= h && h <= 23)
    `for`
      time
    , claim "Minutes number is in range from 0 to 59"
    `true`
      (\t -> let h = Time.minutes t in 0 <= h && h <= 59)
    `for`
      time
    , claim "Seconds number is in range from 0 to 59"
    `true`
      (\t -> let h = Time.seconds t in 0 <= h && h <= 59)
    `for`
      time ]

testSuite = suite "Draft" [dateSuite, timeSuite]

result = quickCheck testSuite

run : IO ()
run =
  display result >>= \success ->
    exit (if success then 0 else 1)

port responses : Signal Response

port requests : Signal Request
port requests = IO.run responses run
