module Cryptography.Caesar (
    eCaesar',
    dCaesar',
    eCaesar,
    dCaesar
) where

import Data.Char
import qualified Cryptography.CryptoCommon as Common

eCaesar' :: Char -> Int -> Char
eCaesar' x k = Common.abc !! (mod ((ord x - ord 'A') + k) 26)

dCaesar' :: Char -> Int -> Char
dCaesar' x k = Common.abc !! (mod ((ord x - ord 'A') - k) 26)

eCaesar :: String -> Int -> String
eCaesar xs k = map (\x -> eCaesar' x k) (Common.preprocess xs)

dCaesar :: String -> Int -> String
dCaesar xs k = map (\x -> dCaesar' x k) (Common.preprocess xs)