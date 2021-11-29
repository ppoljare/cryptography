module Cryptography.Playfair_Matrix (
    makeMatrix,
    matrix
) where

import Data.Char
import qualified Cryptography.CryptoCommon as Common

stringToMatrix :: String -> Int -> String -> [String]
stringToMatrix [] _ acc = [acc]
stringToMatrix xs 5 acc = acc : (stringToMatrix xs 0 [])
stringToMatrix (x:xs) j acc = stringToMatrix xs (j+1) (acc ++ [x])

makeMatrix'' :: String -> Char -> Bool -> String
makeMatrix'' [] _ _ = []
makeMatrix'' (x:xs) c b | x==c && b==False = [x] ++ makeMatrix'' xs c True
                        | x==c && b==True = makeMatrix'' xs c b
                        | otherwise = [x] ++ makeMatrix'' xs c b

makeMatrix' :: String -> String
makeMatrix' xs = foldl (\x y -> makeMatrix'' x y False) xs Common.abc

makeMatrix :: String -> Char -> [String]
makeMatrix key red = stringToMatrix (makeMatrix' (filter (/=red) xs)) 0 []
                   where xs = (Common.preprocess key) ++ Common.abc

matrix' :: [String] -> Char -> Int -> Int -> (Int, Int)
matrix' [] _ _ _ = (-1, -1)
matrix' xs _ 5 _ = (-1, -1)
matrix' xs c i 5 = matrix' xs c (i+1) 0
matrix' xs c i j | x==c = (i,j)
                 | otherwise = matrix' xs c i (j+1)
                 where x = xs !! i !! j

matrix :: [String] -> Char -> (Int, Int)
matrix xs c = matrix' xs (toUpper c) 0 0
