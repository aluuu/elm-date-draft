module Draft.Utils where


clip : comparable -> comparable -> comparable -> comparable
clip a b x =
  if | x < a -> a
     | x > b -> b
     | otherwise -> x
