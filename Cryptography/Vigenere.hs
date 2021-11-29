module Cryptography.Vigenere (
    findTrigrams,
    eVigenere,
    dVigenere,
    findKey
) where

import Data.Char
import qualified Cryptography.CryptoCommon as Common
import qualified Cryptography.Caesar as Caesar
import qualified Cryptography.Vigenere_Decrypt as VD

-------------------------------------------- TRIGRAMS ---------------------------------------------

allTrigrams = [[x] ++ [y] ++ [z] | x <- Common.abc, y <- Common.abc, z <- Common.abc]

findTrigrams'' :: String -> String -> Int -> [Int]
findTrigrams'' [] _ _ = []
findTrigrams'' (x:y:z:xs) trigram i | (x==a && y==b && z==c) = [i] ++ findTrigrams'' xs trigram (i+3)
                                    | otherwise = findTrigrams'' (y:z:xs) trigram (i+1)
                                    where a = trigram !! 0
                                          b = trigram !! 1
                                          c = trigram !! 2
findTrigrams'' _ _ _ = []

findTrigrams' :: String -> [String] -> [(String, [Int])]
findTrigrams' [] _ = []
findTrigrams' _ [] = []
findTrigrams' xs (t:ts) | (length result) > 1 = [(t, result)] ++ findTrigrams' xs ts
                        | otherwise = findTrigrams' xs ts
                        where result = findTrigrams'' xs t 0

findTrigrams :: String -> [(String, [Int])]
findTrigrams xs = findTrigrams' (Common.preprocess xs) allTrigrams

------------------------------------------- GET THE KEY -------------------------------------------

findKey' :: [String] -> String
findKey' [] = []
findKey' (x:xs) = [chr (VD.mg x + ord 'A')] ++ findKey' xs

findKey :: String -> Int -> String
findKey xs m = findKey' (VD.matrix xs m)

---------------------------------------------- MAIN -----------------------------------------------

eVigenere' :: String -> String -> Int -> Int -> String
eVigenere' [] _ _ _ = []
eVigenere' (x:xs) key i len = [Caesar.eCaesar' x k] ++ (eVigenere' xs key (i+1) len)
                              where k = ord (key !! (mod i len)) - ord 'A'

eVigenere :: String -> String -> String
eVigenere xs key = eVigenere' (Common.preprocess xs) (map (\x -> toUpper x) key) 0 (length key)

dVigenere' :: String -> String -> Int -> Int -> String
dVigenere' [] _ _ _ = []
dVigenere' (x:xs) key i len = [Caesar.dCaesar' x k] ++ (dVigenere' xs key (i+1) len)
                              where k = ord (key !! (mod i len)) - ord 'A'

dVigenere :: String -> String -> String
dVigenere xs key = dVigenere' (Common.preprocess xs) (map (\x -> toUpper x) key) 0 (length key)