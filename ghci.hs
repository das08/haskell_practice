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

bmiTell :: (RealFloat a, Show a) => a -> a -> String
bmiTell weight height
    | bmi <= skinny = "BMI: " ++ show bmi ++ "Boney"
    | bmi <= normal = "BMI: " ++ show bmi ++ "Normal"
    | bmi <= fat = "BMI: " ++ show bmi ++ "Obese"
    | otherwise = "BMI: " ++ show (weight/height^2) ++ "Too BIG"
    where bmi = weight / height ^ 2
          skinny = 18.5
          normal = 25.0
          fat = 30.0

calcBmis :: (RealFloat a) => [(a, a)] -> [a]  
calcBmis xs = [bmi w h | (w, h) <- xs]  
    where bmi weight height = weight / height ^ 2  