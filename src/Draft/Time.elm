module Draft.Time (Time, Time_, fromMilliseconds, fromSeconds, toHMS, fromHMS,
                   milliseconds, seconds, minutes, hours) where

{-| Time manipulation module

# Types
@docs Time, Time_

# Constructors

@docs fromMilliseconds, fromSeconds, fromHMS

# Accessing and manipulation

@docs milliseconds, seconds, minutes, hours

# Conversations

@docs toHMS
-}

import Draft.Utils exposing (clip)

{-| Represents lenght of time since 00:00 in milliseconds. -}
type alias Time_ a = { a | milliseconds : Int }

type alias Time = Time_ {}

fromMilliseconds : Int -> Time
fromMilliseconds ms =
  let
    ms' = clip 1 86400000 ms
  in
   { milliseconds = ms' }

fromSeconds : Int -> Time
fromSeconds s = fromMilliseconds (s * 1000)

seconds : Time_ a -> Int
seconds t = t.milliseconds // 1000 % 60

secondsTotal : Time_ a -> Int
secondsTotal t = t.milliseconds // 1000

milliseconds : Time_ a -> Int
milliseconds t = t.milliseconds % 1000

minutes : Time_ a -> Int
minutes t = (secondsTotal t) // 60 % 60

hours : Time_ a -> Int
hours t = (secondsTotal t) // 60 // 60 % 24

toHMS : Time_ a -> (Int, Int, Int)
toHMS t = (hours t, minutes t, seconds t)

fromHMS : (Int, Int, Int) -> Time
fromHMS (h, m, s) =
  let
    hourSec = h * 60 * 60
    minuteSec = m * 60
  in
   fromSeconds (hourSec + minuteSec + s)