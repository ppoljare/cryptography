module Cryptography.Vigenere_Decrypt (
    matrix,
    f,
    ic,
    mmg,
    mg,
    findMax
) where

import Data.Char
import qualified Cryptography.CryptoCommon as Common

----------------------------------------- TEXT TO MATRIX ------------------------------------------

matrix'' :: String -> Int -> Int -> Int -> String
matrix'' [] _ _ _ = []
matrix'' (x:xs) i j m | mod i m == j = [x] ++ matrix'' xs (i+1) j m
                      | otherwise = matrix'' xs (i+1) j m

matrix' :: String -> Int -> Int -> [String]
matrix' xs i m | i==m = []
               | otherwise = (matrix'' xs 0 i m) : (matrix' xs (i+1) m)

matrix :: String -> Int -> [String]
matrix xs m = matrix' xs 0 m

--------------------------------------- CALCULATE PROBABILITIES -----------------------------------

f' :: String -> Char -> Int -> Int
f' [] _ _ = 0
f' (x:xs) y g | x_==y_g = 1 + f' xs y g
              | otherwise = f' xs y g
              where x_ = ord x - ord 'A'
                    y_ = ord y - ord 'A'
                    y_g = mod (y_ - g) 26

f :: String -> Int -> [Int]
f xs g = map (\x -> f' xs x g) Common.abc

---------------------------------------- COINCIDENCE INDEX ----------------------------------------

ic' :: String -> Double
ic' xs = (fromIntegral (sum (zipWith (*) fi fim1))) / (fromIntegral (n*(n-1)))
       where n = length xs
             fi = f xs 0
             fim1 = map (\x -> x-1) fi

ic :: [String] -> [Double]
ic xs = map (\x -> ic' x) xs

--------------------------------------- CALCULATE VALUES M_j --------------------------------------

mmg' :: String -> Int -> Double
mmg' xs g = sum (zipWith (*) Common.p (map (\x -> fromIntegral x) (f xs g))) / (fromIntegral (length xs))

mmg :: String -> [Double]
mmg xs = map (\g -> mmg' xs g) [0..25]

---------------------------------------- FIND THE MAX VALUE ---------------------------------------

findMax' :: [Double] -> Double -> Int -> Int
findMax' [] _ _ = -1
findMax' (x:xs) m i | x==m = i
                    | otherwise = findMax' xs m (i+1)

findMax :: [Double] -> Int
findMax xs = findMax' xs (maximum xs) 0

mg :: String -> Int
mg xs = mod (- findMax (mmg xs)) 26

