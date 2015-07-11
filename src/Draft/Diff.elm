module Draft.Diff where

type alias Diff = {milliseconds:Int}

fromMilliseconds : Int -> Diff
fromMilliseconds ms = {milliseconds=ms}

fromSeconds : Int -> Diff
fromSeconds s = fromMilliseconds s*1000

fromMinutes : Int -> Diff
fromMinutes m = fromSeconds m*60

fromHours : Int -> Diff
fromHours h = fromMinutes m*60

fromDays : Int -> Diff
fromDays d = fromHours d*24
