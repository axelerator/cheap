
module Main where
import System.Environment

import Text.ParserCombinators.Parsec hiding (spaces)
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
  , cMembers :: [Member]
  , cStructors :: [Structor]
  } deriving (Show, Eq)

type Body = String


data Initializers = Initializers String | NoInits
  deriving (Show, Eq)

data Method = Method Visibility Type Identifier ParameterList Body Modifier
  deriving (Show, Eq)

data MethodName = SimpleName String
                | Operator String

data Structor = Constructor String Initializers Body
              | Destructor  String Body
  deriving (Show, Eq)

data ParameterList = ParameterList String
  deriving (Show, Eq)

data Parameter = Parameter Type Identifier
  deriving (Show, Eq)

data IncludeType = Global | Local
  deriving (Show, Eq)

data Include = Include String IncludeType
  deriving (Show, Eq)

data Visibility = Public | Private
  deriving (Show, Eq, Ord)

data Modifier = Default | Const
  deriving (Show, Eq)

data Member = ClassMember Visibility Type Identifier
            | ObjectMember Visibility Type Identifier
  deriving (Show, Eq)

data Type = Type String
  deriving (Show, Eq)

data Identifier = Identifier String
  deriving (Show, Eq)




symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

readExpr :: String -> String
readExpr input = case parse (initializers) "list" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value" ++ (show val)

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
          --putStrLn hed
          --putStrLn $ take 20 $ repeat '-'
          --putStrLn impl
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
  skipMany $ oneOf " "
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
  return $ (vis, Type returnType, (Identifier funcName), parameters, mod)

operators = ["=", "==", "+", "+", "-", "*", "+=", "-=", "*=", "!=", "^", "^=", "/", "/=", "[]", "%"]
operators' = map ((flip (++)) (reverse "operator")) (map reverse operators)
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
    mbidentS = findIndex (not . isIdentC) rvsd
    identS (Just a) = a
    identS Nothing = 1
    (h, t) = splitAt (identS mbidentS) rvsd
    
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
  (Identifier paramType) <- identifier
  _ <- spaces
  paramName <- identifier
  return $ Parameter (Type paramType) paramName

parseList :: Parser ParameterList
parseList = do
  s <- many $ noneOf ")"
  return $ ParameterList s

clazz :: Parser Class
clazz = do
  skipMany $ oneOf "\n" 
  preIncludes <- many $ try include
  skipMany $ oneOf "\n" 
  (Identifier classname) <- classSignature 
  skipMany $ oneOf "\n" 
  includes <- many $ try include
  skipMany $ oneOf "\n" 
  members <- many $ try member
  skipMany $ oneOf "\n" 
  structors <- many (try structor)
  skipMany $ oneOf "\n" 
  methods <- many method
  return $ Class classname preIncludes includes methods  members structors

structor = do
  stype <- try (string "con") <|> (string "de") 
  string "structor"
  params <- many (noneOf "\n")
  char '\n'
  initials <- try  initializers <|> string ""
  let initT = if null initials
                then
                  NoInits
                else
                  Initializers initials
  indent <- many1 (oneOf " ")
  line1 <- many (noneOf "\n")
  char '\n'
  lines <- many (prefixedLines indent)
  char '\n'
  let body = (joinLines . (map ((++) "  "))) (line1:lines)
  case stype of
    "de" ->  return $ Destructor  params body
    "con" -> return $ Constructor params initT body

method = do
  (visibility, returnType, name, parameters, modifier) <- signature

  indent <- many1 (oneOf " ")
  line1 <- many (noneOf "\n")
  char '\n'
  lines <- many (prefixedLines indent)
  char '\n'
  let all = line1:lines
  return $ Method visibility returnType name parameters ((joinLines . (map ((++) "  "))) all) modifier

member = do
  classm <- try  (string "class") <|> string ""
  skipMany (oneOf " ")
  vis <- visibility
  funcNameAntType <- many (noneOf "()\n;")
  let (returnType, funcName) = splitTypeAndName funcNameAntType
  char ';'
  char '\n'
  if classm == "class" then
    return $ ClassMember vis (Type returnType) (Identifier funcName)
    else
    return $ ObjectMember vis (Type returnType) (Identifier funcName)


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
  wrapC <- try (char '"') <|> (char '<')
  filen <- many (noneOf "\">")
  skipMany1 (oneOf "\">")
  skipMany $ oneOf " "
  skipMany $ oneOf "\n"
  return $ Include filen (foo wrapC)

foo :: Char -> IncludeType
foo '"' = Local
foo _ = Global

classSignature = do
  string "class"
  spaces
  classname <- identifier
  return classname

initializers = do
  char ':'
  i <- many $ noneOf "\n"
  char '\n'
  return $ i

upperStr = map toUpper
lowerStr = map toLower

mkImpl :: Class -> String
mkImpl (Class name _ includes methods _ xstructors) =
  joinLines [
    --, joinLines $ map ((joinLines . (implMethod name)) methods
      "#include \"" ++ (lowerStr name) ++ ".h\""
    , finishImplBlock name includes
    , finishImplBlock name xstructors
    , finishImplBlock name methods
    , ""
    ]


mkHeader :: Class -> String
mkHeader  (Class name preIncludes includes methods members structors) =
  joinLines [
      "#ifndef " ++ (upperStr name) ++ "_H"
    , "#define " ++ (upperStr name) ++ "_H"
    , ""
    , renderIncludes preIncludes
    , ""
    , "class " ++ name ++ "{"
    , "public:"
    , finishBlock name structors
    , finishBlock name puMt
    , finishBlock name puC
    , finishBlock name puO
    , ""
    , "private:"
    , finishBlock name prMt
    , finishBlock name prC
    , finishBlock name prO
    --, joinLines $ mkMemberHeader members 
    , "};"
    , ""
    , "#endif"
    ]
  where
    (puC, prC, puO, prO) = mkMemberHeader members
    (puMt, prMt) = splitMethods methods


type Lin = String
type Block = [Lin]

class HeaderContributor a where
  toHeaderLines :: String -> a -> Block

class ImplContributor a where
  toImplLines :: String -> a -> Block

mkHdBlock :: HeaderContributor a => String -> a -> Block
mkHdBlock className x = toHeaderLines className x

mkHdBlocks :: HeaderContributor a => String -> [a] -> [Block]
mkHdBlocks className xs = map (mkHdBlock className) xs

mergeBlocks :: [Block] -> Block
mergeBlocks = foldr (++) []

finishBlock :: HeaderContributor a => String -> [a] -> String
finishBlock className xs =  joinLines $ mergeBlocks $ (mkHdBlocks className) xs

mkImplBlock :: ImplContributor a => String -> a -> Block
mkImplBlock className x = toImplLines className x

mkImplBlocks :: ImplContributor a => String -> [a] -> [Block]
mkImplBlocks className xs = map (mkImplBlock className) xs

mergeImplBlocks :: [Block] -> Block
mergeImplBlocks = foldr (++) []

finishImplBlock :: ImplContributor a => String -> [a] -> String
finishImplBlock className xs =  joinLines $ mergeBlocks $ (mkImplBlocks className) xs


instance HeaderContributor Member where 
  toHeaderLines _ (ObjectMember _ (Type typ) (Identifier name)) = 
   [ "  " ++ typ ++ " " ++ name ++ ";"]
  toHeaderLines _ (ClassMember visibility (Type typ) (Identifier name)) = 
   [ "  " ++ typ ++ " " ++ name ++ ";"]

instance ImplContributor Member where 
  toImplLines _ (ObjectMember _ _ _) = [] 
  toImplLines clazz (ClassMember _ (Type typ) (Identifier name)) = 
   [typ ++ " " ++ clazz ++ "::" ++ name ++ ";"]

instance HeaderContributor Method where
  toHeaderLines _ m = [headerMethod m]

--instance HeaderContributor Include where
--  toHeaderLines _ (Include f Local) = ["#include \"" ++ f ++ "\""]
--toHeaderLines _ (Include f Global) = ["#include <" ++ f ++ ">"]

instance ImplContributor Include where 
  toImplLines _ (Include f Local) = ["#include \"" ++ f ++ "\""]
  toImplLines _ (Include f Global) = ["#include <" ++ f ++ ">"]

instance ImplContributor Method where 
  toImplLines = implMethod 

instance HeaderContributor Structor where
  toHeaderLines = headerStructor

instance ImplContributor Structor where 
  toImplLines = implStructor 

vToStr :: Visibility -> String
vToStr Public  = "public"
vToStr Private = "private"


mkMemberHeader members = (puC, prC, puO, prO)
  where
    (classMs, objectMs) = partition isClass members
    (puC, prC) = partition isPublic classMs
    (puO, prO) = partition isPublic objectMs
    isClass (ClassMember _ _ _) = True
    isClass (ObjectMember _ _ _) = False

isPublic (ObjectMember Public _ _) = True
isPublic (ObjectMember _ _ _) = False

isPublic (ClassMember Public _ _) = True
isPublic (ClassMember _ _ _) = False

renderInitializers NoInits = ""
renderInitializers (Initializers s) = ": " ++ s

headerStructor className (Constructor params _ _) =
  ["  " ++ className ++ params ++ ";"]
headerStructor className (Destructor params _) =
  ["  ~" ++ className ++ params ++ ";"]

headerMethod (Method v (Type returnT) (Identifier name) (ParameterList ps) _ m) =
   "  " ++ returnT ++ " " ++ name ++ "(" ++ ps ++ ")" ++ (modToStr m) ++  ";" 

implMethod className (Method visibility (Type returnT) (Identifier name) (ParameterList ps) body m) =
    [
      returnT ++ "  " ++ className ++ "::" ++ name ++ "(" ++ ps ++ ") " ++ (modToStr m) ++  " {"
    , body
    , "}"
    , ""
    ]

implStructor className (Destructor head body ) =
    [
      className ++ "::~" ++ className ++ head ++ "{"
    , body
    , "}"
    , ""
    ]

implStructor className (Constructor head inits body ) =
    [
      className ++ "::" ++ className ++ head ++ (renderInitializers inits) ++ "{"
    , body
    , "}"
    , ""
    ]

isPubMeth (Method Public  _ _ _ _ _) = True
isPubMeth (Method _ _ _ _ _ _) = False

splitMethods = partition isPubMeth

modToStr Const = "const"
modToStr _ = ""

renderIncludes includes = joinLines $ map mkLne includes
  where 
    mkLne (Include filen Local) = "#include " ++ "\"" ++ filen ++ "\""
    mkLne (Include filen Global) = "#include " ++ "<" ++ filen ++ ">"
    


