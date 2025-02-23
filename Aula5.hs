module Aula5 where


deslocaDireita :: [a] -> Int -> [a]
deslocaDireita l n =
 let t = length l 
     lf = take (t-n) l
     li = drop (t-n) l 
 in li ++ lf 

deslocaE :: [a] -> Int -> [a]
deslocaE l n =
  let t = length l 
      lf = drop (t-n) l
      li = take (t-n) l 
  in lf ++ li 

mydiv :: Int -> Int -> Maybe Int
mydiv x y 
    | x > 0 && y > 0 = Just (div x y)
    | otherwise = Nothing

substitui :: a -> Int -> [a] -> [a]
substitui x n l1
         | n < 0 = l1
         | n > 0 =
            let t = length l1
                li = take (n) l1
                lf = drop (n) l1
            in li ++ [x] ++ lf

substitui :: String ->            


{-
concat1 ::[[a]] -> [a]
concat1 [[ ]] = [ ]
concat1 (x:xs) = [x] ++ concat1 xs

take1 :: Int -> [a] -> [a]
take1 _ [ ] = [ ]
take1 n (x:xs)
    | n <= 0 = [ ]
    | n > 0 = x : take1 (n-1) xs

drop1 :: Int -> [a] -> [a]
drop1 _ [ ] = [ ]
drop1 n (x:xs)
     | n <= 0 = (x:xs)
     | n > 0 = drop1 (n-1) xs

splitAt1 :: Int -> [a] -> ([a], [a])
splitAt1 _ [ ] = [ ]
splitAt1 n (x:xs)
       | n <= 0 = (x:xs)
       | n > 0 = [x] : splitAt1 (n-1) xs

zip1 :: [a] -> [b] -> [(a,b)]
zip1 _ [ ] = [ ]
zip1 [ ] _ = [ ]
zip1 (x:xs) (y:ys) = (x,y) : zip1 xs ys

unzip1 :: [(a,b)] -> ([a], [b])
unzip1 [ ] = [[],[]]
unzip1 ((x,y):xs) = (x:y) ++ unzip1 xs

words1 :: String -> [String] 
words1 " " = [ ]
words1 (x:xs) = [x] : words1 xs 

unwords1 :: [String] -> String
unwords1 [ ] = " "
unwords1 (x:xs) = x ++ " " ++ unwords1 xs
-}