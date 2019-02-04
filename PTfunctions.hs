module PTfunctions where

quadraticVertex a b c = (-b)/(2*a*c)

quadraticDiscriminant a b c = (b * b) - (4 * a * c)

quadraticRealSolutionQty a b c = if quadraticDiscriminant a b c < 0 then 0 
    else if quadraticDiscriminant a b c == 0 then 1
    else 2

convertFtoC f = (f - 32)/1.8

convertCtoF c = (c * 1.8) + 32

convertTemp t u = if u == "c" then (t * 1.8) + 32 else (t - 32)/1.8
