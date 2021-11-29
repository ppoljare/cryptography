module Cryptography.CryptoCommon (
    abc,
    p,
    preprocess,
) where

import Data.Char

abc = ['A'..'Z']
p = [0.115, 0.015, 0.028, 0.037, 0.084, 0.003, 0.016, 0.008, 0.098, 0.051, 0.036, 0.033, 0.031, 0.066, 0.090, 0.029, 0.000, 0.054, 0.056, 0.048, 0.043, 0.035, 0.000, 0.000, 0.000, 0.023]

preprocess :: String -> String
preprocess xs = filter (`elem` abc) (map (\x -> toUpper x) xs)