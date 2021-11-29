import Data.Char
import Data.List

----------------------------------------- COMMON FUNCITONS ----------------------------------------

abc = ['A'..'Z']

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
main :: IO()
main = do let affineCipher = "TFYYX WQNKR NEOPU PFSNV CFRBR UPAWQ POPMZ PTCPT FCRAP TFHIB UFHRW FAPPW NUFCP OPACF CBZPC NTPTA BABMB RPRUA POCNZ OPMZP VNZP"
          let caesarCipher = "PXPXK XENVD RUXVT NLXHY MAXYK XJNXG VRFXM AHW"
          let cipher = affineCipher
          mode <- getLine
          
          if (mode=="count hr") then
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
            putStr "\nGoodbye :)\n\n"