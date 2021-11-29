module Cryptography.Playfair_String (
    splitString,
    listToString
) where

import qualified Cryptography.CryptoCommon as Common

splitString'' :: String -> Char -> [String]
splitString'' [] _ = []
splitString'' (x:[]) c = [[x]++[c]]
splitString'' (x:y:xs) c | x==y = ([x]++[c]) : splitString'' (y:xs) c
                         | otherwise = ([x]++[y]) : splitString'' xs c

splitString' :: String -> (Char, Char) -> Char -> [String]
splitString' xs (t, f) c = splitString'' (map (\x -> if x==f then t else x) (Common.preprocess xs)) c

splitString :: String -> String -> Char -> [String]
splitString [] _ _ = []
splitString xs lang c | lang=="hrv" = splitString' xs ('V', 'W') c
                      | lang=="eng" = splitString' xs ('I', 'J') c
                      | otherwise = []

listToString :: [String] -> String
listToString xs = tail (foldl (\x y -> x++" "++y) [] xs)
