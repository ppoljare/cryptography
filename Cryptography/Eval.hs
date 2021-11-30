module Cryptography.Eval (
    stringToInt,
    eval
) where

import Data.Char

stringToInt' :: String -> Int -> Int
stringToInt' [] acc = acc
stringToInt' (x:xs) acc = stringToInt' xs (10*acc + ord x - ord '0')

stringToInt :: String -> Int
stringToInt xs = stringToInt' xs 0

evalSplitNum :: String -> String -> [String]
evalSplitNum [] acc = [acc]
evalSplitNum (x:xs) acc | elem x ['0'..'9'] = evalSplitNum xs (acc ++ [x])
                        | otherwise = acc : (evalSplitNum xs [])

evalSplitOp :: String -> [Char]
evalSplitOp [] = []
evalSplitOp (x:xs) | elem x ['+', '-'] = x : (evalSplitOp xs)
                   | otherwise = evalSplitOp xs

eval'' :: [Int] -> [Char] -> Int -> Int
eval'' [] [] acc = acc
eval'' [] (op:ops) acc = acc
eval'' (n:nums) [] acc = acc
eval'' (n:nums) (op:ops) acc | op=='+' = eval'' nums ops (acc + n)
                             | op=='-' = eval'' nums ops (acc - n)
                             | otherwise = eval'' (n:nums) ops acc

eval' :: [Int] -> [Char] -> Int
eval' (n:nums) ops = eval'' nums ops n

eval :: String -> Int
eval xs = eval' (map (\x -> stringToInt x) (evalSplitNum xs "")) (evalSplitOp xs)
