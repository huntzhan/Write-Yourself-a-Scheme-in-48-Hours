module Main where
import Control.Monad
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)

import Data.Array


main :: IO ()
main = do args <- getArgs
          putStrLn (readExpr (args !! 0))


symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~#"


readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    -- Right val -> "Found value"
    Right val -> show val


spaces :: Parser ()
spaces = skipMany1 space


data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Vector (Array Int LispVal)
             | Number Integer
             | String String
             | Bool Bool
             deriving (Show)


parseString :: Parser LispVal
parseString = do char '"'
                 x <- many (noneOf "\"")
                 char '"'
                 return $ String x


parseAtom :: Parser LispVal
parseAtom = do first <- letter <|> symbol
               rest <- many (letter <|> digit <|> symbol)
               let atom = [first] ++ rest
               return $ case atom of 
                          "#t" -> Bool True
                          "#f" -> Bool False
                          otherwise -> Atom atom


parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit


parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces


parseDottedList :: Parser LispVal
parseDottedList = do
    head <- endBy parseExpr spaces
    tail <- char '.' >> spaces >> parseExpr
    return $ DottedList head tail


parseQuoted :: Parser LispVal
parseQuoted = do
    char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]


parseGeneralList :: Parser LispVal
parseGeneralList = try parseList <|> parseDottedList


parseVector :: Parser LispVal
parseVector = do string "`#("
                 List x <- parseGeneralList
                 char ')'
                 -- correct but verbose, use listArray to reduce zip.
                 -- return . Vector $ array (0, length x - 1) (zip [0..] x)
                 return . Vector $ listArray (0, length x - 1) x


parseExpr :: Parser LispVal
parseExpr = parseAtom
         <|> parseString
         <|> parseNumber
         <|> parseQuoted
         <|> do char '('
                x <- parseGeneralList
                char ')'
                return x
         <|> parseVector
