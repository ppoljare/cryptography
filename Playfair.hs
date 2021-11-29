module Playfair (
    
) where

import Data.Char
import qualified Cryptography.CryptoCommon as Common
import qualified Cryptography.Playfair_Matrix as PFM
import qualified Cryptography.Playfair_String as PFS

ePlayfair' :: String -> [String] -> String
ePlayfair' [] _ = []
ePlayfair' (x:y:[]) m | xi==yi = [m !! xi !! (mod (xj+1) 5)] ++ [m !! yi !! (mod (yj+1) 5)]
                      | xj==yj = [m !! (mod (xi+1) 5) !! xj] ++ [m !! (mod (yi+1) 5) !! yj]
                      | otherwise = [m !! xi !! yj] ++ [m !! yi !! xj]
                      where xi = fst (PFM.matrix m x)
                            xj = snd (PFM.matrix m x)
                            yi = fst (PFM.matrix m y)
                            yj = snd (PFM.matrix m y)
ePlayfair' _ _ = []

ePlayfair :: String -> String -> String -> Char -> String
ePlayfair xs key lang c = PFS.listToString (map (\x -> ePlayfair' x m) (PFS.splitString xs lang c))
                        where m = PFM.makeMatrix key (if lang=="hrv" then 'W' else 'J')

dPlayfair' :: String -> [String] -> String
dPlayfair' [] _ = []
dPlayfair' (x:y:[]) m | xi==yi = [m !! xi !! (mod (xj-1) 5)] ++ [m !! yi !! (mod (yj-1) 5)]
                      | xj==yj = [m !! (mod (xi-1) 5) !! xj] ++ [m !! (mod (yi-1) 5) !! yj]
                      | otherwise = [m !! xi !! yj] ++ [m !! yi !! xj]
                      where xi = fst (PFM.matrix m x)
                            xj = snd (PFM.matrix m x)
                            yi = fst (PFM.matrix m y)
                            yj = snd (PFM.matrix m y)
dPlayfair' _ _ = []

dPlayfair :: String -> String -> String -> String
dPlayfair xs key lang = PFS.listToString (map (\x -> dPlayfair' x m) (PFS.splitString xs lang 'X'))
                      where m = PFM.makeMatrix key (if lang=="hrv" then 'W' else 'J')

main :: IO()
main = do let m = PFM.makeMatrix "GAUSS" 'J'
          print (m)
          putStr "\n"
          print (ePlayfair' "AB" m)
          print (ePlayfair' "GQ" m)
          print (ePlayfair' "FY" m)
          putStr "\n"
          print (dPlayfair' "UG" m)
          print (dPlayfair' "UO" m)
          print (dPlayfair' "MS" m)