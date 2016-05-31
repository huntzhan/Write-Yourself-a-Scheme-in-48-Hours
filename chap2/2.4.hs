module Main where

import Data.Char (digitToInt)
import Numeric (readInt, readDec, readOct, readHex)

import Control.Monad
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)

main :: IO ()
main = do args <- getArgs
          putStrLn (readExpr (args !! 0))

-- remove symbol #
symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    -- Right val -> "Found value"
    Right val -> show val

spaces :: Parser ()
spaces = skipMany1 space

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
             deriving (Show)


-- the point is that we need the escaped character. 
parseEscaped :: Parser Char
parseEscaped = do char '\\'
                  x <- oneOf ['\\', '\"', 'n', 'r', 't']
                  return $ case x of
                      '\\' -> '\\'
                      '\"' -> '\"'
                      'n' -> '\n'
                      'r' -> '\r'
                      't' -> '\t'

parseString :: Parser LispVal
parseString = do char '"'
                 x <- many (parseEscaped <|> noneOf ['\\', '\"'])
                 char '"'
                 return $ String x


-- split Bool
parseBool :: Parser LispVal
parseBool = do char '#'
               x <- oneOf "tf"
               return . Bool $ case x of
                   't' -> True
                   'f' -> False


parseAtom :: Parser LispVal
parseAtom = do first <- letter <|> symbol
               rest <- many (letter <|> digit <|> symbol)
               let atom = [first] ++ rest
               return . Atom $ atom


-- Change parseNumber to support the Scheme standard for different bases. You may find the readOct and readHex functions useful.
-- The radix prefixes are #b (binary), #o (octal), #d (decimal), and #x (hexadecimal).
-- That's it.
parseNumberRadix2 :: Parser Integer
parseNumberRadix2 = do try $ string "#b"
                       x <- many1 . oneOf $ "01"
                       return . fst . head . readInt 2 (`elem` "01") digitToInt $ x

parseNumberRadix8 :: Parser Integer
parseNumberRadix8 = do try $ string "#o"
                       x <- many1 . oneOf $ ['0'..'7']
                       return . fst . head . readOct $ x

parseNumberRadix10 :: Parser Integer
parseNumberRadix10 = do option [] (string "#d")
                        x <- many1 digit
                        return . fst . head . readDec $ x

parseNumberRadix16 :: Parser Integer
parseNumberRadix16 = do try $ string "#x"
                        x <- many1 . oneOf $ ['0'..'9'] ++ ['a'..'f'] ++ ['A'..'F']
                        return . fst . head . readHex $ x


parseNumber :: Parser LispVal
parseNumber = do x <- (parseNumberRadix2 <|> parseNumberRadix8 <|> parseNumberRadix10 <|> parseNumberRadix16)
                 return . Number $ x


parseExpr :: Parser LispVal
parseExpr =
        parseAtom
        <|> parseString
        <|> parseNumber 
        -- parseBool should be placed after parseNumber, since #f, #t,
        -- #b...
        <|> parseBool
