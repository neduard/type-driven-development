module Average

export
average : String -> Double
average s = (cast (totalLength s)) / (cast (numWords s))
  where
    totalLength : String -> Nat
    totalLength s = sum (map length (words s))

    numWords : String -> Nat
    numWords x = length (words x)
