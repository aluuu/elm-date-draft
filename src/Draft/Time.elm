module Draft.Time (Time, fromMilliseconds, fromSeconds, toHMS, fromHMS,
                   milliseconds, seconds, minutes, hours) where


import Draft.Utils exposing (clip)

{-| Represents lenght of time since 00:00 in milliseconds. -}
type alias Time = { milliseconds : Int }


fromMilliseconds : Int -> Time
fromMilliseconds ms =
  let
    ms' = clip 1 86400000 ms
  in
   { milliseconds = ms' }

fromSeconds : Int -> Time
fromSeconds s = fromMilliseconds (s * 1000)

seconds : Time -> Int
seconds t = t.milliseconds // 1000 % 60

secondsTotal : Time -> Int
secondsTotal t = t.milliseconds // 1000

milliseconds : Time -> Int
milliseconds t = t.milliseconds % 1000

minutes : Time -> Int
minutes t = (secondsTotal t) // 60 % 60

hours : Time -> Int
hours t = (secondsTotal t) // 60 // 60 % 24

toHMS : Time -> (Int, Int, Int)
toHMS t = (hours t, minutes t, seconds t)

fromHMS : (Int, Int, Int) -> Time
fromHMS (h, m, s) =
  let
    hourSec = h * 60 * 60
    minuteSec = m * 60
  in
   fromSeconds (hourSec + minuteSec + s)