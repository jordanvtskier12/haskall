module PTfuncsyntax where

{-
  Examples that make use of some additional syntax features within 
  functions, and use of recursion for writing functions.
  
  *** All functions must now include a type declaration! ***
  
  To help you get started, type declarations will be provided for you 
  in all templates. Any functions you define on your own should include
  a type declaration.
-}

{-
  Create a function called windChillRisk that accepts a temperature in 
  degrees F, and a wind speed in miles per hour and makes a statement
  about the danger of frostbite. 
  
  The formula for wind chill temperature is:
  Twc = 35.74 + 0.6215*Ta - 35.75*v**0.16 + 0.4275*Ta*v**0.16
  where Ta is the temperature (F) and v is the wind speed (mph).
  
  The relationship between wind chill temperature and frostbite danger is:
  Twc >= 16: "Low Risk"
  16 > Twc >= -17: "Moderate Risk"
  -17 > Twc >= -38: "High Risk"
  -38 > Twc >= -53: "Very High Risk"
  -53 > Twc >= -65: "Severe Risk"
  -64 > Twc : "Extreme Risk"
  
  Based on :
  https://www.weather.gov/safety/cold-wind-chill-chart
  http://www.ec.gc.ca/meteo-weather/default.asp?lang=n&n=5FBF816A-1#wc6
  
  Hint: See the BMI example in the Haskell tutorial.
  Hint: Eliminate redundant code!
  Hint: When you test your function with negative numbers, use parentheses.
  
  For example:
  *PTfuncsyntax> windChillRisk (-10) 25
  "High Risk"
  *PTfuncsyntax> windChillRisk 60 10
  "Low Risk"
  *PTfuncsyntax> windChillRisk (-15) 55
  "Severe Risk"
  *PTfuncsyntax> windChillRisk (-15) 50
  "Very High Risk"
-}

windChillRisk :: (Floating a, Ord a) => a -> a -> String
twc t v = 35.74 + 0.6215*t - 35.75*v**0.16 + 0.4275*t*v**0.16

windChillRisk t v 
    | (twc t v) < -64 = "Extreme Risk"
    | (twc t v) < -53 = "Severe Risk" 
    | (twc t v) < -38 = "Very High Risk"
    | (twc t v) < -17 = "High Risk"
    | (twc t v) <  16 = "Moderate Risk"
    |otherwise = "Low Risk"



{-
  Create a function called yoCount that accepts a string and finds 
  the number of times "yo" or "Yo" appears in the string. Use RECURSION!
  
  For example:
  
  *PTfuncsyntax> yoCount "yo"
  1
  *PTfuncsyntax> yoCount "Yo mother is so sweet! yoyo, dog!"
  3  
-}

yoCount :: String -> Integer
-- replace the "-99" bits with your own code below
yoCount [] = 0
yoCount (_:[]) = 0
yoCount ('y':'o':x) = 1 + yoCount x
yoCount ('Y':'o':x) = 1 + yoCount x
yoCount x = yoCount (tail x)

{-
  Create a RECURSIVE function called interestTable that accepts a 
  principal amount, p, an interest rate r (as a percent), and the 
  number of periods (or years) to calculate the growth in principal.
  The function returns a list of numbers, starting with the original
  principal amount, that shows the principal value for each period 
  (rounded to whole dollars). Rounded values are NOT to be used in 
  calculating the next year's principal amount.
 
  Hint: Each year the new principal is computed from the previous
  year's principal according to: pnew = pold*(1 + 0.01*r)
  Hint: Haskell has a built-in round function.
 
  For example:
  
  *PTfuncsyntax> interestTable 100 10 5
  [100,110,121,133,146,161]
-}

interestTable :: (Eq a, Integral a1, Num a, RealFrac a2, Ord a) => a2 -> a2 -> a -> [a1]
interestTable p r n  -- your function here (may need more patterns!)
    | n <= 1 = [round p, round (p + p * r / 100)]
    | otherwise = round p : interestTable (p + p * r / 100) r (n-1)
    



{- 
  Create a function called charToPhoneDigit that accepts an upper and/or
  lower case character and converts it to a number according to the 
  number/letter mapping on a standard phone keypad. Only valid characters
  need to be handled
  Hint: Use pattern matching, guards or case? Which would be best?
  Hint: ABC=2, DEF=3, GHI=4, JKL=5, MNO=6, PQRS=7, TUV=8, WXYZ=9
  For example:
  
  *PTfuncsyntax> charToPhoneDigit 'p'
  7
  *PTfuncsyntax> charToPhoneDigit 'P' 
  7
-}

charToPhoneDigit :: Char -> Int
charToPhoneDigit c 
    | c == 'a' = 2
    | c == 'b' = 2
    | c == 'c' = 2
    | c == 'A' = 2
    | c == 'B' = 2
    | c == 'C' = 2
    | c == 'd' = 3
    | c == 'D' = 3
    | c == 'e' = 3
    | c == 'E' = 3
    | c == 'f' = 3
    | c == 'F' = 3
    | c == 'g' = 4
    | c == 'G' = 4
    | c == 'h' = 4
    | c == 'H' = 4
    | c == 'i' = 4
    | c == 'I' = 4
    | c == 'j' = 5
    | c == 'J' = 5
    | c == 'k' = 5
    | c == 'K' = 5
    | c == 'l' = 5
    | c == 'L' = 5
    | c == 'M' = 6
    | c == 'm' = 6
    | c == 'n' = 6
    | c == 'N' = 6
    | c == 'o' = 6
    | c == 'O' = 6
    | c == 'p' = 7
    | c == 'P' = 7
    | c == 'q' = 7
    | c == 'Q' = 7
    | c == 'r' = 7
    | c == 'R' = 7
    | c == 's' = 7
    | c == 'S' = 7
    | c == 't' = 8
    | c == 'T' = 8
    | c == 'u' = 8
    | c == 'U' = 8
    | c == 'v' = 8
    | c == 'V' = 8
    | c == 'w' = 9
    | c == 'W' = 9
    | c == 'x' = 9
    | c == 'X' = 9
    | c == 'y' = 9
    | c == 'Y' = 9
    | c == 'z' = 9
    | c == 'Z' = 9
    | otherwise = 0


{-
  Create a RECURSIVE function called numListToNum that accepts a list of
  positive or zero, single-digit integers and returns a number with those 
  digits,in the correct order.
  For example:
  
  *PTfuncsyntax> numListToNum [0,1,3,5,0]
  1350
-}

numList :: [Int] -> String
numList [] = []
numList n = (show (head n) ++ numList (tail n))


numListToNum :: [Int] ->  Int
numListToNum n = read (numList n)::Int



  


{- 
  Create a function called wordsToPhone that accepts a string of upper and/or
  lower case characters, possibly including spaces or punctuation characters, 
  and converts it to a number according to the number/letter mapping on a 
  standard phone keypad (ignoring the invalid characters).
  
  Hint: Use charToPhoneDigit and numListToNum. You may want to introduce 
        other small functions to help.
  For example:
  
  *PTfuncsyntax> wordsToPhone "pizza"
  74992
  *PTfuncsyntax> wordsToPhone "Good Morning, Vietnam"
  466366764648438626
-}

wordsToPhone :: String -> Int

wordsToPhone w = numListToNum (fakewordsToPhone w) 
  

fakewordsToPhone :: String -> [Int]
fakewordsToPhone w 
  |w == [] = []
  |charToPhoneDigit (head w) == 0 = fakewordsToPhone (tail w)
  |otherwise = [charToPhoneDigit (head w)] ++ fakewordsToPhone (tail w)
  
