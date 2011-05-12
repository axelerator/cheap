module Main where
import System.Environment

import Text.ParserCombinators.Parsec hiding (spaces)
import Monad
import Data.List
import System.IO
import Data.Char
import Data.Maybe

testStr = "ReturnType myFunc(ParamA paramA) \n  foo\n  bar\n  baz\n"

data Clib = Clib [Class]
  deriving (Show, Eq)

data Class = Class 
  { cName :: String
  , cPreIncludes :: [Include]
  , cIncludes :: [Include] 
  , cMethods :: [Method]
  } deriving (Show, Eq)

data Method = Method Visibility Identifier Identifier ParameterList String Modifier
  deriving (Show, Eq)

data MethodName = SimpleName String
                | Operator String

data ParameterList = ParameterList String
  deriving (Show, Eq)

data Parameter = Parameter Identifier Identifier
  deriving (Show, Eq)

data Include = Include String
  deriving (Show, Eq)

data Visibility = Public | Private
  deriving (Show, Eq)

data Modifier = Default | Const
  deriving (Show, Eq)

type Type = String
data Identifier = Identifier String
  deriving (Show, Eq)




symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

readExpr :: String -> String
readExpr input = case parse (spaces >> parseList) "list" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value"

spaces :: Parser ()
spaces = skipMany1 space

main :: IO ()
main = do 
  args <- getArgs
  fileContent <- readFile (args !! 0)
  let mBclassR = readClass fileContent
  case mBclassR of
    Left err -> do
          putStrLn $ show err
    Right classR -> do
          let hed = mkHeader classR
          let impl = mkImpl classR
          putStrLn hed
          putStrLn $ take 20 $ repeat '-'
          putStrLn impl
          writeFile ((lowerStr (cName classR)) ++ ".h") hed  
          writeFile ((lowerStr (cName classR)) ++ ".c") impl  

readClass :: String -> Either ParseError Class
readClass input = parse clazz "sign" input

identStart = oneOf "_"

identifier :: Parser Identifier
identifier = do
  first <- letter <|> identStart
  rest <- many (letter <|> digit <|> (oneOf "_"))
  let nameStr = first : rest
  return $ Identifier nameStr

visibility = do
  private <- try (string "private") <|> try (string "public") <|> string ""
  return $ visFromStr private

visFromStr "" = Private
visFromStr "private" = Private
visFromStr "public" = Public

modifier = do
  modStr <- try (string "const") <|> string ""
  return $ modFromStr modStr

modFromStr "" = Default
modFromStr "const" = Const


signature = do
  vis <- visibility
  funcNameAntType <- many (noneOf "(")
  let (returnType, funcName) = splitTypeAndName funcNameAntType
  char '('
  parameters <- parseList
  char ')'  
  skipMany $ oneOf " "
  mod <- modifier 
  char '\n'
  return $ (vis, Identifier returnType, (Identifier funcName), parameters, mod)

operators = ["=", "==", "+", "+", "-", "*", "+=", "-=", "*=", "!="]
operators' = map ((flip (++)) (reverse "operator")) operators 
identifierChars = "qwertzuiopasdfghjklyxcvbnmQWERTZUIOPASDFGHJKLYXCVBNM123456789_"

contains xs s = isJust $ find (\c -> c == s) xs

isIdentC = contains identifierChars

splitTypeAndName :: String -> (String, String)
splitTypeAndName s = choose (testOperator s)
  where
    choose Nothing = splitTypeAndName' s
    choose (Just t) = t

splitTypeAndName' :: String -> (String, String)
splitTypeAndName' input = (reverse t, reverse h)
  where
    rvsd = reverse input
    identS = fromJust $ findIndex (not . isIdentC) rvsd
    (h, t) = splitAt identS rvsd
    
-- test if method is an overloaded operator
testOperator :: String -> Maybe (String, String)
testOperator s = if null filtered then
                  Nothing
                  else
                    Just $ head filtered
  where
    both = zip operators' (repeat (reverse s))
    filtered = [ ( (reverse . (drop (length op))) full, reverse op) | (op, full) <- both, op `isPrefixOf` full] 


--declaration :: Parser ()
declaration = do
  l0 <- try (char '_')
  return l0
  

parameter = do
  paramType <- identifier
  _ <- spaces
  paramName <- identifier
  return $ Parameter paramType paramName

parseList :: Parser ParameterList
parseList = do
  s <- many $ noneOf ")"
  return $ ParameterList s

clazz :: Parser Class
clazz = do
  skipMany $ oneOf "\n" 
  preIncludes <- many include
  skipMany $ oneOf "\n" 
  (Identifier classname) <- classSignature 
  skipMany $ oneOf "\n" 
  includes <- many include
  skipMany $ oneOf "\n" 
  methods <- many method
  return $ Class classname preIncludes includes methods

method = do
  (visibility, returnType, name, parameters, modifier) <- signature
  indent <- many1 (oneOf " ")
  line1 <- many (noneOf "\n")
  char '\n'
  lines <- many (prefixedLines indent)
  char '\n'
  let all = line1:lines
  return $ Method visibility returnType name parameters ((joinLines . (map ((++) "  "))) all) modifier


joinLines = (intercalate "\n") 

prefixedLines :: String -> Parser String
prefixedLines indent = do
  string indent
  line1 <- many (noneOf "\n")
  char '\n'
  return line1 

include = do
  string "#include"
  spaces
  char '"'
  filen <- many (noneOf "\"")
  char '"'
  return $ Include filen

classSignature = do
  string "class"
  spaces
  classname <- identifier
  return classname

readPLine :: String -> String
readPLine input = case parse (many (prefixedLines "  ")) "line00" input of
  Left err -> "No match:" ++ (show err)
  Right val -> show val

mkFiles :: Class -> (String, String)
mkFiles (Class name preIncludes includes methods) = undefined

upperStr = map toUpper
lowerStr = map toLower

mkImpl :: Class -> String
mkImpl (Class name _ includes methods) =
  joinLines [
      "#include \"" ++ (lowerStr name) ++ ".h\""
    , joinLines $ map (implMethod name) methods
    , ""
    , renderIncludes includes
    ]


mkHeader :: Class -> String
mkHeader  (Class name preIncludes includes methods) =
  joinLines [
      "#ifndef " ++ (upperStr name) ++ "_H"
    , "#define " ++ (upperStr name) ++ "_H"
    , ""
    , renderIncludes preIncludes
    , ""
    , "class " ++ name ++ "{"
    , ""
    , joinLines $ map headerMethod methods
    , "};"
    , ""
    , "#endif"
    ]

headerMethod (Method v (Identifier returnT) (Identifier name) (ParameterList ps) _ _) =
   "  " ++ returnT ++ " " ++ name ++ "(" ++ ps ++ ");" 

implMethod className (Method visibility (Identifier returnT) (Identifier name) (ParameterList ps) body _) =
  joinLines [
      returnT ++ "  " ++ className ++ "::" ++ name ++ "(" ++ ps ++ ") {"
    , body
    , "}"
    , ""
    ]


renderIncludes includes = joinLines $ map mkLne includes
  where mkLne (Include filen) = "#include " ++ "\"" ++ filen ++ "\""
