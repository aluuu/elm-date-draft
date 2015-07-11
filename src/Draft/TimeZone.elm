module Draft.TimeZone where

type alias TimeZone = {
  minutes : Int,
  summerOnly: Bool,
  name: String}

fromMinutes : Int -> TimeZone
fromMinutes minutes =
  {minutes = minutes,
   summerOnly = False,
   name = ""}

fromHours : Int -> TimeZone
fromHours hours =
  fromMinutes (hours * 60)

utc : TimeZone
utc = {minutes=0,
       summerOnly=False,
       name="UTC"}