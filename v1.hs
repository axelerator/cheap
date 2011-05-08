module Main where
import System.Environment

import Text.ParserCombinators.Parsec hiding (spaces)
import Monad
import Data.List

testStr = "ReturnType myFunc(ParamA paramA) \n  foo\n  bar\n  baz\n"

data Clib = Clib [Class]
  deriving (Show, Eq)

data Class = Class [Method]
  deriving (Show, Eq)

data Method = Method Identifier ParameterList String
  deriving (Show, Eq)

data ParameterList = ParameterList [Parameter]
  deriving (Show, Eq)

data Parameter = Parameter Identifier Identifier
  deriving (Show, Eq)

type Type = String
data Identifier = Identifier String
  deriving (Show, Eq)


symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

readExpr :: String -> String
readExpr input = case parse (spaces >> symbol) "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value"

spaces :: Parser ()
spaces = skipMany1 space

main :: IO ()
main = do 
  args <- getArgs
  putStrLn (readExpr (args !! 0))

readIdentifier :: String -> String
readIdentifier input = case parse method "sign" input of
  Left err -> "No match:" ++ (show err)
  Right val -> show val

identStart = oneOf "_"

identifier :: Parser Identifier
identifier = do
  first <- letter <|> identStart
  rest <- many (letter <|> digit <|> (oneOf "_"))
  let nameStr = first : rest
  return $ Identifier nameStr

signature = do
  returnType <- identifier
  _ <- spaces
  funcName <- identifier
  char '('
  parameters <- parseList
  char ')'  
  skipMany $ oneOf " "
  char '\n'
  return $ (funcName, parameters)

--many0 = optional many1
--many0 = fst . manies

skipOne c = do
  _ <- char c
  return ()

--commaSeparator = (skipMany1 space) >> (skipOne ',') >> (skipMany1 space)
commaSeparator :: Parser ()
commaSeparator = (skipMany1 space)>> (skipOne ',') >> (skipMany1 space)


parameter = do
  paramType <- identifier
  _ <- spaces
  paramName <- identifier
  return $ Parameter paramType paramName

parseList :: Parser ParameterList
parseList = liftM ParameterList $ sepBy parameter commaSeparator

method = do
  (name, parameters) <- signature
  indent <- many1 (oneOf " ")
  line1 <- many (noneOf "\n")
  char '\n'
  --let indent = "  "
  lines <- many (prefixedLines indent)
  let all = line1:lines
  --let lines = []
  return $ Method name parameters (joinLines all)

joinLines = intercalate "\n" 

prefixedLines :: String -> Parser String
prefixedLines indent = do
  string indent
  line1 <- many (noneOf "\n")
  char '\n'
  return line1 


readPLine :: String -> String
readPLine input = case parse (many (prefixedLines "  ")) "line00" input of
  Left err -> "No match:" ++ (show err)
  Right val -> show val


