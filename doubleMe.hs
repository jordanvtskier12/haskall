
fizzBuzz n f b = [if x `mod`(f*b) == 0 then "FizzBuzz"
  else 
    if x `mod` f == 0 then "Fizz"
      else
        if x `mod` b == 0 then "Buzz"
          else show x
            | x <- [1..n]] 
