import Data.Char
import Data.List

----------------------------------------- COMMON FUNCITONS ----------------------------------------

abc = ['A'..'Z']
abc3 = [[x] ++ [y] ++ [z] | x <- abc, y <- abc, z <- abc]

removeSpaces :: String -> String
removeSpaces [] = []
removeSpaces (x:xs) | elem x ['a'..'z'] = [toUpper x] ++ (removeSpaces xs)
                    | elem x ['A'..'Z'] = [x] ++ (removeSpaces xs)
                    | otherwise = removeSpaces xs

separateLetters :: String -> String
separateLetters [] = []
separateLetters (x:[]) = [x]
separateLetters (x:xs) = [x] ++ " " ++ (separateLetters xs)

inverse' :: [Int] -> [Int] -> Int -> Int
inverse' [] _ _ = 0
inverse' (x:xs) (y:ys) i | x==i = y
                         | otherwise = inverse' xs ys i

inverse :: Int -> Int
inverse i = inverse' xs ys i
            where xs = [1, 3, 5, 7, 9, 11, 15, 17, 19, 21, 23, 25]
                  ys = [1, 9, 21, 15, 3, 19, 7, 23, 11, 5, 17, 25]

countLetters' :: String -> Char -> Int
countLetters' [] _ = 0
countLetters' (x:xs) c | elem c [x, (toUpper x), (toLower x)] = 1 + (countLetters' xs c)
                       | otherwise = countLetters' xs c

sortLetters :: [(Char, Int)] -> [(Char, Int)]
sortLetters [] = []
sortLetters (x:xs) = sortLetters a ++ [x] ++ sortLetters b
                     where a = [y | y <- xs, (snd x) <= (snd y)]
                           b = [y | y <- xs, (snd x) > (snd y)]

countLetters :: String -> [(Char, Int)]
countLetters xs = sortLetters (zip abc [countLetters' xs i | i <- abc])

removeNonExistent :: [(String, Int)] -> [(String, Int)]
removeNonExistent [] = []
removeNonExistent (x:xs) | (snd x) > 0 = [x] ++ removeNonExistent xs
                         | otherwise = removeNonExistent xs

countTrigrams' :: String -> String -> Int
countTrigrams' [] _ = 0
countTrigrams' (x:y:z:xs) trigram | (x==a && y==b && z==c) = 1 + countTrigrams' xs trigram
                                  | otherwise = countTrigrams' (y:z:xs) trigram
                                  where a = trigram !! 0
                                        b = trigram !! 1
                                        c = trigram !! 2
countTrigrams' _ _ = 0

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
findTrigrams [] = []
findTrigrams xs = findTrigrams' xs abc3

--countTrigrams :: String -> [(String, Int)]
--countTrigrams [] = []

------------------------------------------ CAESAR CIPHER ------------------------------------------

eCaesar' :: Char -> Int -> Char
eCaesar' x k = abc !! (mod ((ord x - ord 'A') + k) 26)

dCaesar' :: Char -> Int -> Char
dCaesar' x k = abc !! (mod ((ord x - ord 'A') - k) 26)

eCaesar :: String -> Int -> String
eCaesar xs k = map (\x -> eCaesar' x k) (removeSpaces xs)

dCaesar :: String -> Int -> String
dCaesar xs k = map (\x -> dCaesar' x k) (removeSpaces xs)

solveCaesar :: String -> (Char, Char) -> String
solveCaesar cipher (x, y) = dCaesar cipher k
                            where k = ord y - ord x

----------------------------------------- VIGENERE CIPHER -----------------------------------------

eVigenere' :: String -> String -> Int -> Int -> String
eVigenere' [] _ _ _ = []
eVigenere' (x:xs) key i len = [eCaesar' x k] ++ (eVigenere' xs key (i+1) len)
                              where k = ord (key !! (mod i len)) - ord 'A'

eVigenere :: String -> String -> String
eVigenere xs key = eVigenere' (removeSpaces xs) (map (\x -> toUpper x) key) 0 len
                   where len = length key

dVigenere' :: String -> String -> Int -> Int -> String
dVigenere' [] _ _ _ = []
dVigenere' (x:xs) key i len = [dCaesar' x k] ++ (dVigenere' xs key (i+1) len)
                              where k = ord (key !! (mod i len)) - ord 'A'

dVigenere :: String -> String -> String
dVigenere xs key = dVigenere' (removeSpaces xs) (map (\x -> toUpper x) key) 0 len
                   where len = length key

{-solveVigenere' :: String -> String -> Int -> Int -> String
solveVigenere' [] _ _ _ = []
solveVigenere' 

solveVigenere :: String -> String -> String
solveVigenere cipher key = solveVigenere' cipher key 0 len
                           where len = length key-}

------------------------------------------ AFFINE CIPHER ------------------------------------------

eAffine' :: Char -> Int -> Int -> Char
eAffine' x a b = abc !! (mod ((ord x - ord 'A') * a + b) 26)

eAffine :: String -> Int -> Int -> String
eAffine xs a b = map (\x -> eAffine' x a b) (removeSpaces xs)

dAffine' :: Char -> Int -> Int -> Char
dAffine' x a b = abc !! (mod ((inverse a)*(ord x - ord 'A' - b)) 26)

dAffine :: String -> Int -> Int -> String
dAffine xs a b = map (\x -> dAffine' x a b) (removeSpaces xs)

findSolutionAffine'' :: (Int, Int) -> (Int, Int) -> (Int, Int) -> Bool
findSolutionAffine'' (a, b) (x1, y1) (x2, y2) = ((mod (a * x1 + b) 26) == y1) && ((mod (a * x2 + b) 26) == y2)

findSolutionAffine' :: [(Int, Int)] -> (Char, Char) -> (Char, Char) -> (Int, Int)
findSolutionAffine' [] _ _ = (0, 0)
findSolutionAffine' (ab:asbs) (xs1, ys1) (xs2, ys2) | (findSolutionAffine'' (a, b) (x1, y1) (x2, y2)) == True = ab
                                                    | otherwise = findSolutionAffine' asbs (xs1, ys1) (xs2, ys2)
                                                    where a = fst ab
                                                          b = snd ab
                                                          x1 = ord xs1 - ord 'A'
                                                          y1 = ord ys1 - ord 'A'
                                                          x2 = ord xs2 - ord 'A'
                                                          y2 = ord ys2 - ord 'A'

findSolutionAffine :: (Char, Char) -> (Char, Char) -> (Int, Int)
findSolutionAffine (x1, y1) (x2, y2) = findSolutionAffine' [(a, b) | a <- alist, b <- [0..25]] (x1, y1) (x2, y2)
                                       where alist = [1, 3, 5, 7, 9, 11, 15, 17, 19, 21, 23, 25]

solveAffine :: String -> (Char, Char) -> (Char, Char) -> String
solveAffine cipher (x1, y1) (x2, y2) = dAffine cipher a b
                                       where a = fst res
                                             b = snd res
                                             res = findSolutionAffine ((toUpper x1), (toUpper y1)) ((toUpper x2), (toUpper y2))

----------------------------------------------- MAIN ----------------------------------------------
mainDV :: String -> IO()
mainDV cipher = do key <- getLine
                   if (key==":quit") then
                     putStr "\nGoodbye :)\n\n"
                   else if (key==":main") then
                     main
                   else
                     do print (dVigenere cipher key)
                        mainDV cipher

main :: IO()
main = do let vigenereCipher = "JFELLQVVRTPVNNVZXVTEFDMQBQGZAGVTRVIMBWZQZVSEEUMHQIKZUCPWKFXXVTIGQCVXRFXBWMAPAGJSGTANONVDCFUEMKIFJJITZNONVVWWJNVYCQJSMTARPHAPLAFDRXRROIQLAIBOFDTBWSFEDBSIEZOGFOJEI"
          let cipher = vigenereCipher
          --let cipher = "UCOOA VWVJO OVGVF KRVNB BPQQB FRYMJ MFGZZ RGZQG WGBJO UAMXJ HUAVN EXKOF OFJXQ AXDSF VREFC QZZIK CMZRY ZXOWG BJOUR VGVYU TZDFD UTZTC TGJVM JYOPJ NJHGD RMVFG HCBTW ZMGHJ HAXBX QOOHI TUIQT ANTSB IGHDC ICIBA SIQZE ZTXIQ T"
          --let cipher = "MTFEQHMJEVLAENRSHAWNSKWHNQHGWGYHSONDSKMSVAJEANWMTRXGETBWLMTXYHJZVTMLEHTGNMWIKLMBTAKEANWKYFXSNDNWAXVVVSFNROGBMWIFMHVSNAWRSVBIQRWREZNWFIMEKNEXSKDSISGLZBVWFSTXWCWGE"
          mode <- getLine
          
          if (mode=="ft") then
            do print (findTrigrams (removeSpaces cipher))
               putStr "\n\n"
               main
          else if (mode=="e") then
            do text <- getLine
               key <- getLine
               print (eVigenere text key)
          else if (mode=="d") then
            do cipher <- getLine
               key <- getLine
               print (dVigenere cipher key)
          else if (mode=="dv") then
            {-do key <- getLine
               if (key==":quit") then
                 putStr "\nGoodbye :)\n\n"
               else
                do print (dVigenere cipher key)
                   main-}
            mainDV cipher
          else if (mode=="quit") then
            putStr "\nGoodbye :)\n\n"
          else
            do main
          {-if (mode=="count hr") then
            do print (countLetters cipher)
               putStr "\n"
               print ([(a,b) | a <- "AIOEN", b <- (map (\x -> fst x) (take 5 (countLetters cipher)))])
          else if (mode=="count en") then
            do print (countLetters cipher)
               putStr "\n"
               print ([(a,b) | a <- "ETAOI", b <- (map (\x -> fst x) (take 5 (countLetters cipher)))])
          else if (mode=="solve a") then
            do input <- getLine
               let xy1 = read input :: (Char, Char)
               input <- getLine
               let xy2 = read input :: (Char, Char)
               print (solveAffine cipher xy1 xy2)
          else if (mode=="solve c") then
            do input <- getLine
               let xy = read input :: (Char, Char)
               print (solveCaesar cipher xy)
          else
            putStr "\nGoodbye :)\n\n"-}
          --print (findTrigrams (removeSpaces cipher))