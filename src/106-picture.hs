module MyPicture where
import Data.Char
import Data.List

sayit :: String -> IO ()
sayit = putStr . say

say :: String -> String
say [] = ""
say s = pictureToString (stringToPicture s) 

pictureToString :: [[Char]] -> [Char]
pictureToString [] = "\n"
pictureToString (x:xs) = (x ++ "\n") ++ (pictureToString xs)

stringToPicture :: [Char] -> [[Char]]
stringToPicture [] = []
stringToPicture (x:xs) = mySideBySide (charToPicture x) (stringToPicture xs)

mySideBySide :: [[Char]] -> [[Char]] -> [[Char]]
mySideBySide xs [] = xs
mySideBySide [] ys = ys
mySideBySide (x:xs) (y:ys) = (x ++ y):(mySideBySide xs ys)

charToPicture :: Char -> [[Char]]
charToPicture ch
  | ch == 'a' || ch == 'A' = ["           ", "     A A A ", "   A     A ", " A       A ", " A       A ", " A A A A A ", " A       A ", " A       A ", "           "]
  | ch == 'b' || ch == 'B' = ["           ", " B B B B   ", " B       B ", " B       B ", " B B B B   ", " B       B ", " B       B ", " B B B B   ", "           "]
  | ch == 'c' || ch == 'C' = ["           ", "   C C C   ", " C       C ", " C         ", " C         ", " C         ", " C       C ", "   C C C   ", "           "] 
  | ch == 'd' || ch == 'D' = ["           ", " D D D D   ", " D       D ", " D       D ", " D       D ", " D       D ", " D       D ", " D D D D   ", "           "]
  | ch == 'e' || ch == 'E' = ["           ", " E E E E E ", " E         ", " E         ", " E E E E   ", " E         ", " E         ", " E E E E E ", "           "]
  | ch == 'f' || ch == 'f' = ["           ", " F F F F F ", " F         ", " F         ", " F F F F   ", " F         ", " F         ", " F         ", "           "]
  | ch == 'g' || ch == 'G' = ["           ", "   G G G   ", " G       G ", " G         ", " G         ", " G     G G ", " G       G ", "   G G G   ", "           "]
  | ch == 'h' || ch == 'H' = ["           ", " H       H ", " H       H ", " H       H ", " H H H H H ", " H       H ", " H       H ", " H       H ", "           "]
  | ch == 'i' || ch == 'I' = ["           ", "   I I I   ", "     I     ", "     I     ", "     I     ", "     I     ", "     I     ", "   I I I   ", "           "]
  | ch == 'j' || ch == 'J' = ["           ", "   J J J J ", "         J ", "         J ", "         J ", "         J ", " J       J ", "   J J J   ", "           "]
  | ch == 'k' || ch == 'K' = ["           ", " K       K ", " K     K   ", " K   K     ", " KK        ", " K   K     ", " K     K   ", " K       K ", "           "]
  | ch == 'l' || ch == 'L' = ["           ", " L         ", " L         ", " L         ", " L         ", " L         ", " L         ", " L L L L L ", "           "]
  | ch == 'm' || ch == 'M' = ["           ", " M       M ", " M M   M M ", " M   M   M ", " M   M   M ", " M       M ", " M       M ", " M       M ", "           "]
  | ch == 'n' || ch == 'N' = ["           ", " N       N ", " N       N ", " N N     N ", " N   N   N ", " N     N N ", " N       N ", " N       N ", "           "]
  | ch == 'o' || ch == 'O' = ["           ", "   O O O   ", " O       O ", " O       O ", " O       O ", " O       O ", " O       O ", "   O O O   ", "           "]
  | ch == 'p' || ch == 'P' = ["           ", " P P P P   ", " P       P ", " P       P ", " P P P P   ", " P         ", " P         ", " P         ", "           "]
  | ch == 'q' || ch == 'Q' = ["           ", "   Q Q Q   ", " Q       Q ", " Q       Q ", " Q       Q ", " Q       Q ", "   Q Q Q   ", "         Q ", "           "]
  | ch == 'r' || ch == 'R' = ["           ", " R R R R   ", " R       R ", " R       R ", " R R R R   ", " R   R     ", " R      R  ", " R       R ", "           "]
  | ch == 's' || ch == 'S' = ["           ", "   S S S   ", " S       S ", " S         ", "   S S S   ", "         S ", " S       S ", "   S S S   ", "           "]
  | ch == 't' || ch == 'T' = ["           ", " T T T T T ", "     T     ", "     T     ", "     T     ", "     T     ", "     T     ", "     T     ", "           "]
  | ch == 'u' || ch == 'U' = ["           ", " U       U ", " U       U ", " U       U ", " U       U ", " U       U ", " U       U ", "   U U U   ", "           "]
  | ch == 'v' || ch == 'V' = ["           ", " U       U ", " U       U ", " U       U ", " U       U ", " U       U ", "   V   V   ", "     V     ", "           "]
  | ch == 'w' || ch == 'W' = ["           ", " W       W ", " W       W ", " W       W ", " W   W   W ", " W   W   W ", " W   W   W ", "  WW   WW  ", "           "]
  | ch == 'x' || ch == 'X' = ["           ", " X       X ", " X       X ", "   X   X   ", "     X     ", "   X   X   ", " X       X ", " X       X ", "           "]
  | ch == 'y' || ch == 'Y' = ["           ", " Y       Y ", " Y       Y ", " Y       Y ", "   Y Y Y Y ", "         Y ", " Y       Y ", "   Y Y Y   ", "           "]
  | ch == 'z' || ch == 'Z' = ["           ", " Z Z Z Z Z ", "         Z ", "       Z   ", "     Z     ", "   Z       ", " Z         ", " Z Z Z Z Z ", "           "]
  | ch == '1'              = ["           ", "     1     ", "   1 1     ", " 1   1     ", "     1     ", "     1     ", "     1     ", " 1 1 1 1 1 ", "           "]
  | ch == '2'              = ["           ", "   2 2 2   ", " 2       2 ", "         2 ", "       2   ", "     2     ", "   2       ", " 2 2 2 2 2 ", "           "]
  | ch == '3'              = ["           ", "   3 3 3   ", " 3       3 ", "         3 ", "     3 3   ", "         3 ", " 3       3 ", "   3 3 3   ", "           "]
  | ch == '4'              = ["           ", "       4   ", "     4 4   ", "   4   4   ", " 4     4   ", " 4 4 4 4 4 ", "       4   ", "       4   ", "           "]
  | ch == '5'              = ["           ", " 5 5 5 5 5 ", " 5         ", " 5 5 5 5   ", "         5 ", "         5 ", " 5       5 ", "   5 5 5   ", "           "]
  | ch == '6'              = ["           ", "   6 6 6   ", " 6       6 ", " 6         ", " 6 6 6 6   ", " 6       6 ", " 6       6 ", "   6 6 6   ", "           "]
  | ch == '7'              = ["           ", " 7 7 7 7 7 ", "         7 ", "       7   ", "     7     ", "     7     ", "     7     ", "     7     ", "           "]
  | ch == '8'              = ["           ", "   8 8 8   ", " 8       8 ", " 8       8 ", "   8 8 8   ", " 8       8 ", " 8       8 ", "   8 8 8   ", "           "]
  | ch == '9'              = ["           ", "   9 9 9   ", " 9       9 ", " 9       9 ", "   9 9 9 9 ", "         9 ", " 9       9 ", "   9 9 9   ", "           "] 
  | ch == '0'              = ["           ", "   0 0 0   ", " 0       0 ", " 0     0 0 ", " 0   0   0 ", " 0 0     0 ", " 0       0 ", "   0 0 0   ", "           "]
  | ch == ' '              = ["           ", "           ", "           ", "           ", "           ", "           ", "           ", "           ", "           "]     