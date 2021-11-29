import Data.List
import Data.Char

------------------------------ DEFINE LISTS ------------------------------

abc = ['A'..'Z']
p = [0.115, 0.015, 0.028, 0.037, 0.084, 0.003, 0.016, 0.008, 0.098, 0.051, 0.036, 0.033, 0.031, 0.066, 0.090, 0.029, 0.000, 0.054, 0.056, 0.048, 0.043, 0.035, 0.000, 0.000, 0.000, 0.023]
allTrigrams = [[x] ++ [y] ++ [z] | x <- abc, y <- abc, z <- abc]

------------------------------ CAESAR CIPHER ------------------------------

eCaesar' :: Char -> Int -> Char
eCaesar' x k = chr (ord (mod (x+k) 26) + ord 'A')

eCaesar :: String -> Int -> String
eCaesar xs k = map (\x -> eCaesar' x k) xs

dCaesar' :: Char -> Int -> Char
dCaesar' x k = chr (ord (mod (x-k) 26) + ord 'A')

dCaesar :: String -> Int -> String
dCaesar xs k = map (\x -> dCaesar' x k) xs

------------------------------ VIGENERE CIPHER ------------------------------

eVigenere' :: String -> String -> Int -> Int -> String
eVigenere' [] _ _ _ = []
eVigenere' (x:xs) key i d = [eCaesar' x k] ++ eVigenere' xs key (i+1)
                          where k = ord (key !! (mod i d)) - ord 'A'

eVigenere :: String -> String -> String
eVigenere xs key = eVigenere' xs key 0 (length key)

dVigenere' :: String -> String -> Int -> Int -> String
dVigenere' [] _ _ _ = []
dVigenere' (x:xs) key i d = [dCaesar' x k] ++ dVigenere' xs key (i+1)
                          where k = ord (key !! (mod i d)) - ord 'A'

dVigenere :: String -> String -> String
dVigenere xs key = dVigenere' xs key 0 (length key)

------------------------------ FIND TRIGRAMS ------------------------------

findTrigrams'' :: String -> String -> Int -> [Int]
findTrigrams'' [] _ _ = []
findTrigrams'' (x:y:z:xs) trigram i | x==a && y==b && z==c = [i] ++ findTrigrams'' xs trigram (i+3)
                                    | otherwise = findTrigrams'' (y:z:xs) trigram (i+1)
									where a = trigram !! 0
									      b = trigram !! 1
										  c = trigram !! 2

findTrigrams' :: String -> String -> [(String, [Int])]
findTrigrams' [] _ = []
findTrigrams' xs trigram | length res > 1 = [(trigram, res)]
                         | otherwise = []
						 res = findTrigrams'' xs trigram 0

findTrigrams :: String -> [(String, [Int])]
findTrigrams xs = map (\x -> findTrigrams' xs x) allTrigrams

------------------------------ TEXT TO MATRIX ------------------------------

matrix'' :: String -> Int -> Int -> Int -> String
matrix'' [] _ _ _ = []
matrix'' (x:xs) i j m | mod i m == j = [x] ++ matrix'' xs (i+1) j m
                      | otherwise = matrix'' xs (i+1) j m

matrix' :: String -> Int -> Int -> [String]
matrix' xs i m | i==m = []
               | otherwise = (matrix'' xs 0 i m) : (matrix' xs (i+1) m)

matrix :: String -> Int -> [String]
matrix xs m = matrix' xs 0 m

------------------------------ CALCULATE PROBABILITIES ------------------------------

f' :: String -> Char -> Int -> Int
f' [] _ _ = 0
f' (x:xs) y g | x_==y_g = 1 + f' xs y g
              | otherwise = f' xs y g
              where x_ = ord x - ord 'A'
                    y_ = ord y - ord 'A'
                    y_g = mod (y_ - g) 26

f :: String -> Int -> [Int]
f xs g = map (\x -> f' xs x g) abc

------------------------------ COINCIDENCE INDEX ------------------------------

ic' :: String -> Float
ic' xs = (fromIntegral (sum (zipWith (*) fi fim1))) / (fromIntegral (n*(n-1)))
       where n = length xs
             fi = f xs 0
             fim1 = map (\x -> x-1) fi

ic :: [String] -> [Int]
ic xs = map (\x -> round ((ic' x)*1000)) xs

------------------------------ CALCULATE VALUES M_j ------------------------------

mmg' :: String -> Int -> Float
mmg' xs g = sum (zipWith (*) p (map (\x -> fromIntegral x) (f xs g))) / (fromIntegral (length xs))

mmg :: String -> [Float]
mmg xs = map (\g -> mmg' xs g) [0..25]

------------------------------ FIND THE MAX VALUE ------------------------------

findMax' :: [Float] -> Float -> Int -> Int
findMax' [] _ _ = -1
findMax' (x:xs) m i | x==m = i
                    | otherwise = findMax' xs m (i+1)

findMax :: [Float] -> Int
findMax xs = findMax' xs (maximum xs) 0

mg :: String -> Int
mg xs = mod (- findMax (mmg xs)) 26

------------------------------ GET THE KEY ------------------------------

findKey :: [String] -> String
findKey [] = []
findKey (x:xs) = [chr (mg x + ord 'A')] ++ findKey xs

------------------------------ MAIN ------------------------------

main :: IO()
main = do let cipher = "JFELLQVVRTPVNNVZXVTEFDMQBQGZAGVTRVIMBWZQZVSEEUMHQIKZUCPWKFXXVTIGQCVXRFXBWMAPAGJSGTANONVDCFUEMKIFJJITZNONVVWWJNVYCQJSMTARPHAPLAFDRXRROIQLAIBOFDTBWSFEDBSIEZOGFOJEI"
          let m = 6
          let res = matrix cipher m
          let prvi = res !! 0
          print (mmg' prvi 0)
          print (findKey res)