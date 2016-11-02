--Zack Mryyan
 
decodeText:: String -> String
decodeText str = decypherText (splitOn "M" (fromMorseCode str)) (splitString (fromMorseCode str)) 

splitString:: String -> String
splitString f 
 |(f !! 1) == 'M' = tail (tail f)
 |otherwise = splitString (tail f)

decypherText:: [String] -> String -> String
decypherText xs str = decodeCipher str (stringToInt (xs !! 0))

stringToInt:: String -> Int
stringToInt str = findKey (map charToInt str)

findKey:: [Int] -> Int
findKey xs
 |(length xs) == 1 = (xs !! 0)
 |(length xs) == 2 = ((xs !! 0) * 10) + (xs !! 1)


charToInt:: Char -> Int
charToInt val
 |val == '0' = 0
 |val == '1' = 1
 |val == '2' = 2
 |val == '3' = 3
 |val == '4' = 4
 |val == '5' = 5
 |val == '6' = 6
 |val == '7' = 7
 |val == '8' = 8
 |val == '9' = 9
  
 
fromMorseCode:: String -> String
fromMorseCode a = morseToChar (splitOn " " a)

morseToChar:: [String] -> String
morseToChar b = map convertCode b

convertCode:: String -> Char
convertCode c 
  |c == ".-" = 'A'
  |c == "-..." = 'B'
  |c == "-.-." = 'C'
  |c == "-.." = 'D'
  |c == "." = 'E'
  |c == "..-." = 'F' 
  |c == "--." = 'G'
  |c == "...." = 'H'
  |c == ".." = 'I'
  |c == ".---" = 'J'
  |c == "-.-" = 'K'
  |c == ".-.." = 'L'
  |c == "--" = 'M'
  |c == "-." = 'N'
  |c == "---" = 'O'
  |c == ".--." = 'P'
  |c == "--.-" = 'Q'
  |c == ".-." = 'R'
  |c == "..." = 'S'
  |c == "-" = 'T'
  |c == "..-" = 'U'
  |c == "...-" = 'V'
  |c == ".--" = 'W'
  |c == "-..-" = 'X'
  |c == "-.--" = 'Y'
  |c == "--.." = 'Z'
  |c == ".----" = '1'
  |c == "..---" = '2'
  |c == "...--" = '3'
  |c == "....-" = '4'
  |c == "....." = '5'
  |c == "-...." = '6'
  |c == "--..." = '7'
  |c == "---.." = '8'
  |c == "----." = '9'
  |c == "-----" = '0'  
  |otherwise = ' '
  
decodeCipher::String -> Int -> String
decodeCipher str 0 = str
decodeCipher str key = decodeCipher (map cipherShift str) (key-1)
 
cipherShift::Char -> Char
cipherShift c   
  |c == 'A' = 'B'
  |c == 'B' = 'C'
  |c == 'C' = 'D'
  |c == 'D' = 'E'
  |c == 'E' = 'F' 
  |c == 'F' = 'G'
  |c == 'G' = 'H'
  |c == 'H' = 'I'
  |c == 'I' = 'J'
  |c == 'J' = 'K'
  |c == 'K' = 'L'
  |c == 'L' = 'M'
  |c == 'M' = 'N'
  |c == 'N' = 'O'
  |c == 'O' = 'P'
  |c == 'P' = 'Q'
  |c == 'Q' = 'R'
  |c == 'R' = 'S'
  |c == 'S' = 'T'
  |c == 'T' = 'U'
  |c == 'U' = 'V'
  |c == 'V' = 'W'
  |c == 'W' = 'X'
  |c == 'X' = 'Y'
  |c == 'Y' = 'Z'
  |c == 'Z' = '1'
  |c == '1' = '2'
  |c == '2' = '3'
  |c == '3' = '4'
  |c == '4' = '5'
  |c == '5' = '6'
  |c == '6' = '7'
  |c == '7' = '8'
  |c == '8' = '9'
  |c == '9' = '0' 
  |c == '0' = 'A'  
  |otherwise = ' '

  