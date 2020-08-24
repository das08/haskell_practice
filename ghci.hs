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

calcBmis2 :: (RealFloat a) => [(a, a)] -> [a]
calcBmis2 xs = [bmi | (w, h) <- xs, let bmi = w*h^2]

cylinder :: (RealFloat a) => a -> a -> a  
cylinder r h = 
    let sideArea = 2 * pi * r * h  
        topArea = pi * r ^2  
    in  sideArea + 2 * topArea  

maximum' ::(Ord a) => [a] -> a
maximum' [] = error "empty list"
maximum' [x] = x
maximum' (x:xs)
    | x > maxTail = x
    | otherwise = maxTail
    where maxTail = maximum' xs  

maximum'2 ::(Ord a) => [a] -> a
maximum'2 [] = error "empty list"
maximum'2 [x] = x
maximum'2 (x:xs) = max x (maximum'2 xs)

replicate' ::(Num i, Ord i) => i -> a -> [a]
replicate' n x
    | n<=0 = []
    | otherwise = x:replicate' (n-1) x