import Data.Char
import Data.List
import qualified Cryptography.Vigenere as Vigenere
import qualified Cryptography.Eval as Eval

----------------------------------------------- MAIN ----------------------------------------------
mainDV :: String -> IO()
mainDV cipher = do putStr "Decryption mode > "
                   input <- getLine
                   if (input=="help" || input==":help") then
                     do putStr "Input the desired key length and the program will generate the most likely key of that length.\n"
                        putStr "commands:\n"
                        putStr "  :main - return to Main\n"
                        putStr "  :quit - quit the program\n\n"
                        mainDV cipher
                   else if (input==":quit") then
                     putStr "\nGoodbye :)\n\n"
                   else if (input==":main") then
                     main
                   else
                     do let m = read input :: Int
                        let key = Vigenere.findKey cipher m
                        putStr "Key: "
                        putStr key
                        putStr "\nPlaintext: "
                        putStr (Vigenere.dVigenere cipher key)
                        putStr "\n\n"
                        mainDV cipher

main :: IO()
main = do let vigenereCipher = "JFELLQVVRTPVNNVZXVTEFDMQBQGZAGVTRVIMBWZQZVSEEUMHQIKZUCPWKFXXVTIGQCVXRFXBWMAPAGJSGTANONVDCFUEMKIFJJITZNONVVWWJNVYCQJSMTARPHAPLAFDRXRROIQLAIBOFDTBWSFEDBSIEZOGFOJEI"
          --let strapacCipher = "HBQAFDTRDIJILJHORIRARSGDZMSOUTTPDKCZZSLFTIQAQJGUOEWOOPNSWURKTPUVKPTTDUROUIMEUTHOSIUAMJHSWSSAYZCSHFUITAMJHKQJHJHNWDHOYIUENDMEFNDZDMLEMECAUVZKRSNOUORTXOQEQOITDKVTC"
          let cipher = vigenereCipher
          putStr "main > "
          mode <- getLine
          
          if (mode=="help") then
            do putStr "commands:\n"
               putStr "  ft - find trigrams\n"
               putStr "  e - encrypt: input plain text and key\n"
               putStr "  d - decrypt: input cipher and key\n"
               putStr "  dv - enter Decryption mode\n"
               putStr "  dvi - enter Decryption mode with input\n"
               putStr "  eval - evaluate string\n"
               putStr "  :quit - quit the program\n\n"
               main
          else if (mode=="ft") then
            do putStr "Searching...\n"
               print (Vigenere.findTrigrams cipher)
               putStr "\n"
               main
          else if (mode=="e") then
            do putStr "  plaintext > "
               text <- getLine
               putStr "  key > "
               key <- getLine
               putStr "Cipher: "
               putStr (Vigenere.eVigenere text key)
               putStr "\n\n"
               main
          else if (mode=="d") then
            do putStr "  cipher > "
               cipher <- getLine
               putStr "  key > "
               key <- getLine
               putStr "Plaintext: "
               putStr (Vigenere.dVigenere cipher key)
               putStr "\n\n"
               main
          else if (mode=="dv") then
            mainDV cipher
          else if (mode=="dvi") then
            do putStr "  cipher > "
               input <- getLine
               mainDV input
          else if (Eval.findEval mode == True) then
            do putStr "  result : "
               print (Eval.eval (Eval.getEvalInput mode))
               putStr "\n"
               main
          else if (mode==":quit") then
            putStr "Goodbye :)\n\n"
          else
            do putStr "Unknown command.\n"
               main
          