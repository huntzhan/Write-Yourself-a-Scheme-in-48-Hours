module Main where
import Control.Monad
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)


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
             | Number Integer
             | String String
             | Bool Bool
             | Empty
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


-- sepEndBy: separated and optionally ended by sep.
parseList :: Parser LispVal
parseList = liftM List $ sepEndBy parseExpr spaces


parseDottedSuffix :: Parser LispVal
parseDottedSuffix = do
    suffix <- option Empty (char '.' >> spaces >> parseExpr)
    return suffix


parseQuoted :: Parser LispVal
parseQuoted = do
    char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]


parseExpr :: Parser LispVal
parseExpr = parseAtom
         <|> parseString
         <|> parseNumber
         <|> parseQuoted
         <|> do char '('
                List x <- parseList
                suffix <- parseDottedSuffix
                char ')'
                return $ case suffix of
                             Empty -> List x
                             otherwise -> DottedList x suffix
