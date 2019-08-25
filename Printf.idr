module Printf

data Format = Number Format
            | Str Format
            | Lit Char Format  -- do we really need String literals?
            | Chr Format       -- from exercise 2
            | Dbl Format
            | End

total PrintfType : Format -> Type
PrintfType (Number fmt) = Int -> PrintfType fmt  -- NOTE: it would be nice if
                                                 -- we could specify
                                                 -- Num a => a -> PrintfType fmt
PrintfType (Str fmt) = String -> PrintfType fmt
PrintfType (Lit lit fmt) = PrintfType fmt
PrintfType End = String  -- teeechnically it should be IO ().
                         -- sprintf would have type ... -> String
PrintfType (Chr fmt) = Char -> PrintfType fmt
PrintfType (Dbl fmt) = Double -> PrintfType fmt

printfFmt : (fmt : Format) -> (acc : String) -> PrintfType fmt
printfFmt (Number fmt) acc = \i => printfFmt fmt (acc ++ show i)
printfFmt (Str fmt) acc = \s => printfFmt fmt (acc ++ s)
printfFmt (Lit lit fmt) acc = printfFmt fmt (acc ++ cast lit)
printfFmt End acc = acc
printfFmt (Chr fmt) acc = \c => printfFmt fmt (acc ++ cast c)
printfFmt (Dbl fmt) acc = \d => printfFmt fmt (acc ++ show d)

toFormat : List Char -> Format
toFormat [] = End
toFormat ('%' :: 'd' :: cs) = Number (toFormat cs)
toFormat ('%' :: 's' :: cs) = Str (toFormat cs)
toFormat ('%' :: 'c' :: cs) = Chr (toFormat cs)
toFormat ('%' :: 'f' :: cs) = Dbl (toFormat cs)
toFormat (c :: cs) = Lit c (toFormat cs)  -- sorry, I'm lazy

printf : (fmt : String) -> PrintfType (toFormat (unpack fmt))
printf fmt = printfFmt _ ""

printfExample : String
printfExample = printf "%d %d      %%%%%%s" 10 2 "foo"

-- NOTEs:
-- Using Lit Char instead of Lit String made things easier when building
-- literals.  I'm curious about the efficiency though...

-- There's still a bit of dependency between format (PrintfType) and
-- toFormat.  For example there is no guarantee that toFormat
-- will produce a case for all possible Formats.
-- I wonder if there are languages where we can specify that a function
-- must be surjective.

-- Let's go back to Chapter6.idr
