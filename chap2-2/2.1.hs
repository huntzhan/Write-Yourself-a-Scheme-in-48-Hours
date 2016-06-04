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


-- wrong, backquote is not only for list.
--
-- support backquote. (@ is not supported.)
-- parseCommaElement :: Parser LispVal
-- parseCommaElement = do char ','
--                        x <- parseExpr
--                        return . List $ [Atom "backquote_comma", x]
-- 
-- parseBackquoteElement :: Parser LispVal
-- parseBackquoteElement = try parseExpr
--                      <|> parseCommaElement
-- 
-- 
-- parseBackquoteList :: Parser LispVal
-- parseBackquoteList = liftM List $ sepBy parseBackquoteElement spaces


-- seems correct.

parseUnquoted :: Parser LispVal
parseUnquoted = do char ','
                   x <- parseExpr
                   return . List $ [Atom "unquoted", x]


parseQuasiquote :: Parser LispVal
parseQuasiquote = do char '`'
                     x <- parseExpr
                     return . List $ [Atom "quasiquote", x]


parseExpr :: Parser LispVal
parseExpr = parseAtom
         <|> parseString
         <|> parseNumber
         <|> parseQuoted
         <|> do char '('
                x <- try parseList <|> parseDottedList
                char ')'
                return x
         <|> parseUnquoted
         <|> parseQuasiquote
