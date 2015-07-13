module Draft.DateTime where

import Draft.Date as T exposing ( Date, Date' )
import Draft.Time as T exposing ( Time, Time' )
import Draft.TimeZone exposing ( TimeZone, utc )
import Draft.Diff exposing ( Diff )

type alias DateTime = Date' (Time' {tz: TimeZone})

fromDateAndTime : Date -> Time -> DateTime
fromDateAndTime d t = {
  julianDay=d.julianDay,
  milliseconds=t.milliseconds,
  tz=utc}

add : Diff -> DateTime -> DateTime
add diff datetime =
  let
    totalTime = diff.milliseconds + datetime.milliseconds
    newTime = if totalTime > 86400000 then totalTime % 86400000 else totalTime
    secondsToAdd = if totalTime > 86400000 then totalTime // 86400000 else 0
    daysToAdd = secondsToAdd // 86400
  in
    {milliseconds = newTime,
     julianDay = datetime.julianDay + daysToAdd,
     tz = datetime.tz}

subtract : Diff -> DateTime -> DateTime
subtract diff datetime =
  let
    invDiff = {milliseconds = -diff.milliseconds}
  in
    add invDiff datetime
