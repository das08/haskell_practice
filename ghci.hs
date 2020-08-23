factorial ::Integer -> Integer
factorial n = product [1..n]

lucky :: (Integral a) => a -> String
lucky 7 ="LUCKY!"
lucky 8 = error "aaa"
lucky y ="Bad"

factorial2 ::(Integral a) => a -> a
factorial2 0 = 1
factorial2 n = n*factorial2 (n-1)

sum' ::(Num a) => [a] -> a
sum' [] = 0
sum' (x:y) = x + sum' y

capital ::String -> String
capital "" = "Empty"
capital all@(x:xs) = "First letter of" ++ all ++ "is" ++ [x]

