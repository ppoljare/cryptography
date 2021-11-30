module Cryptography.Commands (
    printVM,
    findString,
    getStringArg
) where

printVM :: [String] -> IO()
printVM [] = putStr "\n"
printVM (x:xs) = do putStr x
                    putStr "\n"
                    printVM xs

findString :: String -> String -> Bool
findString _ [] = True
findString (x:xs) (t:ts) | x==t = findString xs ts
                         | otherwise = False

getStringArg :: String -> String -> String
getStringArg [] _ = []
getStringArg (x:xs) [] | x==' ' = getStringArg xs []
                       | otherwise = x:xs
getStringArg (x:xs) (t:ts) | x==t = getStringArg xs ts
                           | otherwise = []