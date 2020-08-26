factorial ::Integer -> Integer
factorial n = product [1..n]

lucky :: (Integral a) => a -> String
lucky 7 ="LUCKY!"
lucky 8 = error "aaa"
lucky y ="Bad"

factorial2 ::(Integral a) => a -> a
factorial2 0 = 1
factorial2 n = n*factorial2 (n-1)

-- sum' ::(Num a) => [a] -> a
-- sum' [] = 0
-- sum' (x:y) = x + sum' y

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

take' ::(Num i, Ord i, Eq a) => i -> [a] -> [a]
take' n x
    | n<=0 = []
    | x==[] = []
take' n (x:xs) = x : take' (n-1) xs  

quicksort ::(Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs)=
    let smallerNum = quicksort [a | a<-xs, a<=x]
        biggerNum = quicksort [a | a<-xs, a>x]
    in smallerNum ++ [x] ++ biggerNum

multThree :: (Num a) => a -> a -> a -> a  
multThree x y z = x * y * z  

div10 ::(Floating a) => a -> a
div10 = (/10)

isDigit ::(Num a,Enum a,Eq a) => a -> Bool
isDigit = (`elem` [1..9])

applyTwice :: (a -> a) -> a -> a  
applyTwice f x = f (f x)  

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]  
zipWith' _ [] _ = []  
zipWith' _ _ [] = []  
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys  

filter' ::(a -> Bool) -> [a] -> [a]
filter' _ [] =[]
filter' f (x:xs)
    | f x = x: filter' f xs
    |otherwise = filter' f xs

largestDivisible :: (Integral a) => a  
largestDivisible = head (filter p [100000,99999..])
    where p x = x `mod` 3839 == 0

chain ::(Integral a) => a -> [a]
chain 1 =[1]
chain n
    | odd n = n: chain (n*3 +1)
    | even n = n: chain (n `div` 2)

sum' ::(Num a) => [a] -> a
sum' xs = foldl (\acc x -> acc + x) 0 xs

sum2 ::(Num a) => [a] -> a
sum2 = foldl (+) 0

elem' ::(Eq a) => a -> [a] -> Bool
elem' x = foldl (\acc a -> if a==x then True else acc) False