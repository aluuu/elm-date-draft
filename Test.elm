module Main exposing (..)

import Check exposing (claim, true, that, is, for)
import Check.Test exposing (..)
import Check.Producer
  exposing
    ( Producer
    , rangeInt
    , float
    , int
    , char
    , lowerCaseChar
    , upperCaseChar
    )
import Draft.Date as Date exposing (Date(..), fromJulianDay)
import Draft.Time as Time exposing (Time(Milliseconds), fromMilliseconds)
import Shrink exposing (shrink)
import String
import Random
import ElmTest exposing (runSuiteHtml)


date : Producer Date
date =
  let
    shrinker (Days julianDay) =
      fromJulianDay `Shrink.map` (Shrink.int julianDay)

    generator =
      Random.map (\d -> fromJulianDay (abs d)) int.generator
  in
    Producer generator shrinker


time : Producer Time
time =
  let
    shrinker (Milliseconds ms) =
      fromMilliseconds `Shrink.map` (Shrink.int ms)

    generator =
      Random.map (\ms -> fromMilliseconds (abs ms)) int.generator
  in
    Producer generator shrinker


dateSuite : Check.Claim
dateSuite =
  Check.suite "Draft.Date"
    [ claim "Day number in ordinal date is between 1 and 366"
        `true`
          (\d ->
            let
              ( y, yd ) =
                Date.toOrdinalDate d
            in
              1 <= yd && yd <= 366
          )
        `for` date
    , claim "Month day number is between 1 and 31"
        `true`
          (\d ->
            let
              md =
                Date.day d
            in
              1 <= md && md <= 31
          )
        `for` date
    , claim "Month number is between 1 and 12"
        `true`
          (\d ->
            let
              m =
                Date.month d
            in
              1 <= m && m <= 12
          )
        `for` date
    , claim "Year number should be greater or equal than 1858"
        `true`
          (\d ->
            let
              y =
                Date.year d
            in
              1858 <= y
          )
        `for` date
    ]


timeSuite : Check.Claim
timeSuite =
  Check.suite "Draft.Time"
    [ claim "Hours number is in range from 0 to 23"
        `true`
          (\t ->
            let
              h =
                Time.hours t
            in
              0 <= h && h <= 23
          )
        `for` time
    , claim "Minutes number is in range from 0 to 59"
        `true`
          (\t ->
            let
              h =
                Time.minutes t
            in
              0 <= h && h <= 59
          )
        `for` time
    , claim "Seconds number is in range from 0 to 59"
        `true`
          (\t ->
            let
              h =
                Time.seconds t
            in
              0 <= h && h <= 59
          )
        `for` time
    ]


testSuite : Check.Claim
testSuite =
  Check.suite "Draft" [ dateSuite, timeSuite ]


quickCheckTestSuite : ElmTest.Test
quickCheckTestSuite =
  testSuite |> Check.quickCheck |> evidenceToTest


main : Program Never
main =
  runSuiteHtml quickCheckTestSuite
