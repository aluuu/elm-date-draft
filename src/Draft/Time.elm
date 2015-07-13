module Draft.Time (Time, Time', fromMilliseconds, fromSeconds, toHMS, fromHMS,
                   milliseconds, seconds, minutes, hours) where

{-| Time manipulation module

# Types
@docs Time, Time'

# Constructors

@docs fromMilliseconds, fromSeconds, fromHMS

# Accessing and manipulation

@docs milliseconds, seconds, minutes, hours

# Conversations

@docs toHMS
-}

import Draft.Utils exposing (clip)

{-| Represents lenght of time since 00:00 in milliseconds. -}
type alias Time' a = { a | milliseconds : Int }

type alias Time = Time' {}

fromMilliseconds : Int -> Time
fromMilliseconds ms =
  let
    ms' = clip 1 86400000 ms
  in
   { milliseconds = ms' }

fromSeconds : Int -> Time
fromSeconds s = fromMilliseconds (s * 1000)

seconds : Time' a -> Int
seconds t = t.milliseconds // 1000 % 60

secondsTotal : Time' a -> Int
secondsTotal t = t.milliseconds // 1000

milliseconds : Time' a -> Int
milliseconds t = t.milliseconds % 1000

minutes : Time' a -> Int
minutes t = (secondsTotal t) // 60 % 60

hours : Time' a -> Int
hours t = (secondsTotal t) // 60 // 60 % 24

toHMS : Time' a -> (Int, Int, Int)
toHMS t = (hours t, minutes t, seconds t)

fromHMS : (Int, Int, Int) -> Time
fromHMS (h, m, s) =
  let
    hourSec = h * 60 * 60
    minuteSec = m * 60
  in
   fromSeconds (hourSec + minuteSec + s)