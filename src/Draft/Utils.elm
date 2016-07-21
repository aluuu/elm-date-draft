module Draft.Utils exposing (clip)


clip : comparable -> comparable -> comparable -> comparable
clip a b x =
  if x < a then
    a
  else if x > b then
    b
  else
    x
