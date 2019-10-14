module Main where

import           Data.Char          (digitToInt, intToDigit, isDigit, isSpace)
import           Data.List          (sort)
import           Data.Map           (Map)
import qualified Data.Map           as Map
import           Lib
import           Numeric            (showHex, showIntAtBase)
import           System.Environment
import           System.IO

initialSymbolTable = Map.fromList[
    ("SP", 0),
    ("LCL", 1),
    ("ARG", 2),
    ("THIS", 3),
    ("THAT", 4),
    ("SCREEN", 16384),
    ("KBD", 24576),
    ("R0", 0),
    ("R1", 1),
    ("R2", 2),
    ("R3", 3),
    ("R4", 4),
    ("R5", 5),
    ("R6", 6),
    ("R7", 7),
    ("R8", 8),
    ("R9", 9),
    ("R10", 10),
    ("R11", 11),
    ("R12", 12),
    ("R13", 13),
    ("R14", 14),
    ("R15", 15)]


main :: IO ()
main = do
    [file] <- getArgs
    content <- readFile file
    let codeLines = filter isCode (map deleteSpaceChars (lines content))
    -- print codeLines
    let symbolTable = fillSymbolTable initialSymbolTable codeLines 0
    -- print symbolTable
    let code = convert symbolTable (filter (not . isLabel) codeLines) []
    -- print code
    writeFile (file ++ ".hack") (unlines code)

deleteSpaceChars :: String -> String
deleteSpaceChars x = takeWhile ('/'/=) (filter (not . isSpace) x)

isComment :: String -> Bool
isComment [] = False
isComment s  = head s == '/'

isEmpty :: String -> Bool
isEmpty [] = True
isEmpty _  = False

isCode :: String -> Bool
isCode line = not (isComment line || isEmpty line)

isLabel :: String -> Bool
isLabel []     = False
isLabel (x:xs) = x == '('

type SymbolTable = Map String Int
fillSymbolTable :: SymbolTable -> [String] -> Int -> SymbolTable
fillSymbolTable t [] n = t
fillSymbolTable t (x:xs) n = if isLabel x
    then fillSymbolTable (Map.insert (tail . init $ x) n t) xs n
    else fillSymbolTable t xs (n+1)

convert :: SymbolTable -> [String] -> [String] -> [String]
convert _ [] r                               = r
convert symbolTable (x:xs) r | head x == '@' = convert updatedSymbolTable xs (r ++ [convertACommand updatedSymbolTable a])
    where
        updatedSymbolTable = update symbolTable a
        a = drop 1 x
convert symbolTable (x:xs) r                 = convert symbolTable xs (r ++ [convertCCommand x])

update :: SymbolTable -> String -> SymbolTable
update symbolTable a
    | all isDigit a               = symbolTable
    | Map.notMember a symbolTable = Map.insert a nextAddress symbolTable
    | otherwise                   = symbolTable
    where
        nextAddress = getNextAddress symbolTable

getNextAddress :: SymbolTable -> Int
getNextAddress symbolTable = findFirst list
    where list = sort $ foldr (:) [] symbolTable

findFirst :: [Int] -> Int
findFirst (x : xs@(y:_)) | y == x     = findFirst xs
findFirst (x : xs@(y:_)) | y - x == 1 = findFirst xs
findFirst (x : xs@(y:_)) | y - x > 1  = x + 1
findFirst x              = error ("findFirst:\n" ++ show x)

convertACommand :: SymbolTable -> String -> String
convertACommand symbolTable a
    | all isDigit a            = to16Bit $ convertToBinary (read a :: Int) ""
    | Map.member a symbolTable = to16Bit $ convertToBinary (Map.findWithDefault 0 a symbolTable) ""
    | otherwise                = error ""
    where
        convertToBinary = showIntAtBase 2 intToDigit

to16Bit :: String -> String
to16Bit x | length x < 16 = to16Bit ('0':x)
to16Bit x = x

convertCCommand :: String -> String
convertCCommand x = "111" ++ comp x ++ dest x ++ jump x

dest :: String -> String
dest x | '=' `elem` x = [a,d,m]
    where
        c = takeWhile ('='/=) x
        a = if 'A' `elem` c then '1' else '0'
        m = if 'M' `elem` c then '1' else '0'
        d = if 'D' `elem` c then '1' else '0'
dest _ = "000"

jump :: String -> String
jump x | ';' `elem` x = toJump j
    where j = drop 1 $ dropWhile (';'/=) x
jump _ = "000"

toJump :: String -> String
toJump "JGT" = "001"
toJump "JEQ" = "010"
toJump "JGE" = "011"
toJump "JLT" = "100"
toJump "JNE" = "101"
toJump "JLE" = "110"
toJump "JMP" = "111"
toJump _     = error "toJump"

comp :: String -> String
comp [] = "0000000"
comp x
    | '=' `elem` x && ';' `elem` x = toComp $ withoutJump $ withoutDest x
    | '=' `elem` x = toComp $ withoutDest x
    | ';' `elem` x = toComp $ withoutJump x
    | otherwise = toComp x
    where
        withoutJump = takeWhile (';'/=)
        withoutDest = drop 1 . dropWhile ('='/=)

toComp :: String -> String
toComp "0"   = "0101010"
toComp "1"   = "0111111"
toComp "-1"  = "0111010"
toComp "D"   = "0001100"
toComp "A"   = "0110000"
toComp "M"   = "1110000"
toComp "!D"  = "0001101"
toComp "!A"  = "0110001"
toComp "!M"  = "1110001"
toComp "-D"  = "0001111"
toComp "-A"  = "0110011"
toComp "-M"  = "1110011"
toComp "D+1" = "0011111"
toComp "A+1" = "0110111"
toComp "M+1" = "1110111"
toComp "D-1" = "0001110"
toComp "A-1" = "0110010"
toComp "M-1" = "1110010"
toComp "D+A" = "0000010"
toComp "D+M" = "1000010"
toComp "D-A" = "0010011"
toComp "D-M" = "1010011"
toComp "A-D" = "0000111"
toComp "M-D" = "1000111"
toComp "D&A" = "0000000"
toComp "D&M" = "1000000"
toComp "D|A" = "0010101"
toComp "D|M" = "1010101"
toComp _     = error "toComp"
