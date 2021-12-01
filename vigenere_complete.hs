import Data.Char
import Data.List
import qualified Cryptography.Vigenere as Vigenere
import qualified Cryptography.Vigenere_Decrypt as VD
import qualified Cryptography.Eval as Eval
import qualified Cryptography.Commands as CC

----------------------------------------------- MAIN ----------------------------------------------
mainM :: [String] -> IO()
mainM [] = do putStr "DONE\n"
mainM (x:xs) = do let mj = VD.mmg x
                  putStr "M: "
                  print (map (\x -> (fromIntegral (round (x*10000)))/(fromIntegral 10000)) mj)
                  putStr ( "\nM[j]: " ++ (show ((fromIntegral (round ((maximum mj)*10000)))/(fromIntegral 10000))) )
                  putStr (",  j: " ++ (show (VD.findMax mj)))
                  putStr (",  -j%26: " ++ (show (mod (-VD.findMax mj) 26)))
                  putStr ("\n" ++ ['-' | i <- [1..100]] ++ "\n")
                  mainM xs

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
          let cipher = vigenereCipher
          putStr "main > "
          mode <- getLine
          
          ---------- HELP ----------
          if (mode=="help") then
            do putStr "commands:\n"
               putStr "  ft - find trigrams\n"
               putStr "  e - encrypt: input plain text and key\n"
               putStr "  d - decrypt: input cipher and key\n"
               putStr "  matrix <m> - print letters matrix for m\n"
               putStr "  ic <m> - calculate coincidence index for m\n"
               putStr "  M <m> - calculate M_j for m and print max(M_j)\n"
               putStr "  dv - enter Decryption mode\n"
               putStr "  dvi - enter Decryption mode with input\n"
               putStr "  eval <x> - evaluate string x\n"
               putStr "  :quit - quit the program\n\n"
               main
          
          ---------- FIND TRIGRAMS ----------
          else if (mode=="ft") then
            do putStr "Searching...\n"
               print (Vigenere.findTrigrams cipher)
               putStr "\n"
               main
          
          ---------- ENCRYPT ----------
          else if (mode=="e") then
            do putStr "  plaintext > "
               text <- getLine
               putStr "  key > "
               key <- getLine
               putStr "Cipher: "
               putStr (Vigenere.eVigenere text key)
               putStr "\n\n"
               main
          
          ---------- DECRYPT ----------
          else if (mode=="d") then
            do putStr "  cipher > "
               cipher <- getLine
               putStr "  key > "
               key <- getLine
               putStr "Plaintext: "
               putStr (Vigenere.dVigenere cipher key)
               putStr "\n\n"
               main
          
          ---------- STRING TO MATRIX ----------
          else if (CC.findString mode "matrix" == True) then
            do let m = Eval.eval (CC.getStringArg mode "matrix")
               CC.printVM (VD.matrix cipher m)
               main
          
          ---------- CALCULATE IC ----------
          else if (CC.findString mode "ic" == True) then
            do let m = Eval.eval (CC.getStringArg mode "ic")
               let ics = (map (\x -> (fromIntegral (round (x*10000)))/(fromIntegral 10000)) (VD.ic (VD.matrix cipher m)))
               print (ics)
               main
          
          ---------- CALCULATE M_j ----------
          else if (CC.findString mode "M" == True) then
            do let m = Eval.eval (CC.getStringArg mode "M")
               let matrix = VD.matrix cipher m
               CC.printVM matrix
               mainM matrix
               main
          
          ---------- DECRYPTION MODE ----------
          else if (mode=="dv") then
            do mainDV cipher
          
          ---------- DECRYPTION MODE (WITH INPUT) ----------
          else if (mode=="dvi") then
            do putStr "  cipher > "
               input <- getLine
               mainDV input
          
          ---------- EVALUATE STRING ----------
          else if (CC.findString mode "eval" == True) then
            do putStr "  result : "
               print (Eval.eval (CC.getStringArg mode "eval"))
               putStr "\n"
               main
          
          ---------- QUIT ----------
          else if (mode==":quit") then
            do putStr "Goodbye :)\n\n"
          
          ---------- UNKNOWN COMMAND ----------
          else
            do putStr "Unknown command.\n"
               main