{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Parser where


import           Data.Text (Text, unpack)
import           NeatInterpolation              ( text )
import           Control.Monad                  ( void )
import           Control.Monad.Combinators.Expr
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer    as L
import AST

type Parser = Parsec Void String

sc :: Parser ()
sc = L.space space1 lineCmnt blockCmnt
 where
  lineCmnt  = L.skipLineComment "//"
  blockCmnt = L.skipBlockComment "/*" "*/"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

-- | 'parens' parses something between parenthesis.

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

bracers :: Parser a -> Parser a
bracers = between (symbol "[") (symbol "]")

-- | 'integer' parses an integer.

integer :: Parser Integer
integer = lexeme L.decimal

float :: Parser Double
float = lexeme L.float

-- | 'semi' parses a semicolon.

semi :: Parser String
semi = symbol ";"

comma :: Parser String
comma = symbol ","

rword :: String -> Parser ()
rword w = (lexeme . try) (string w *> notFollowedBy alphaNumChar)

rws :: [String] -- list of reserved words
rws =
  [ "if"   , "then"   , "else"   , "while"
  , "do"   , "skip"   , "true"   , "false"
  , "list"   , "["   , "!"   , "and"
  , "or"   , "def"   , "Class"   , "return"
  , "elif"   , "Call"   , "Void"   , "in"
  , "lambda"
  ]

identifier :: Parser String
identifier = (lexeme . try) (p >>= check)
 where
  p = (:) <$> letterChar <*> many alphaNumChar
  check x = if x `elem` rws
    then fail $ "keyword " ++ show x ++ " cannot be an identifier"
    else return x

whileParser :: Parser Stmt
whileParser = between sc eof stmt


stmt :: Parser Stmt
stmt = f <$> sepBy1 stmt' semi
  where
    -- if there's only one stmt return it without using ‘Seq’
        f l = if length l == 1 then head l else Seq l

stmt' :: Parser Stmt
stmt' = try ifStmt 
 <|> try whileStmt <|> try assignStmt
 <|> try assignTerm <|> try callFunc
 <|> try listPars <|> try (parens stmt)
 <|> try (bracers stmt) <|> try defFunk
 <|> try defClass <|> try lambFunc
 <|> try return'

defFunk :: Parser Stmt
defFunk = do
	rword "def"
	name <- identifier
	arg <- parens (sepBy pyExpr comma)
	void <- symbol ":"
	body <- stmt
	return (Function name arg body) 

callFunc :: Parser Stmt
callFunc = do
  name1 <- identifier
  void (symbol "(")
  arg <- sepBy pyExpr comma
  void (symbol ")")
  return (CallFunc name1 arg)

lambFunc :: Parser Stmt
lambFunc = do
  rword "lambda"
  arg <- sepBy identifier comma
  void (symbol ":")
  body <- pyExpr
  return (LambdaFunc arg body) 

return' :: Parser Stmt
return' = do
	rword "return"
	expr <- pyExpr
	return (Return expr)

listPars :: Parser Stmt
listPars = do
  arg <- bracers (sepBy pyExpr comma)
  return (ListCon arg)


defClass :: Parser Stmt
defClass = do
	rword <- "Class"
	name <- identifier
	void <- symbol ":"
	state <- stmt
	return (Class name state)

ifStmt :: Parser Stmt
ifStmt = do
  rword "if"
  cond <- pyExpr
  void (symbol ":")
  stmt1 <- stmt
  nextStmt <- try (rword "elif" >> elifStmt) <|> (rword "else" >> stmt)
  return (If cond stmt1 nextStmt)

elifStmt :: Parser Stmt
elifStmt = do
	cond <- pyExpr
	void (symbol ":")
	stmt1 <- stmt
	nextStmt <- try (rword "elif" >> elifStmt) <|> (rword "else" >> stmt)
	return (If cond stmt1 nextStmt)

whileStmt :: Parser Stmt
whileStmt = do
  rword "while"
  cond <- pyExpr
  void (symbol ":")
  stmt1 <- stmt
  return (While cond stmt1)

-- for == foreach in python
forStmt :: Parser Stmt
forStmt = do 
	rword "for"
	exp <- pyExpr
	rword "in"
	cont <- pyExpr 
	stmt1 <- stmt
	return (For exp cont stmt1)

assignTerm :: Parser Stmt
assignTerm = do
  var <- identifier
  void <- (symbol "=")
  expr <- pyExpr
  return (Assign var (Left expr))

assignStmt :: Parser Stmt
assignStmt = do
  var <- identifier
  void <- (symbol "=")
  expr <- (try listPars <|> try callFunc <|> try lambFunc)
  return (Assign var (Right expr))

pyExpr :: Parser PTerm
pyExpr = try (makeExprParser term eOperators) <|> term
    
eOperators :: [[Operator Parser PTerm]]
eOperators =
	[
      [Prefix (Not <$ rword  "not")]
    ,

    [  
        InfixL (BiOp AsMod <$ symbol "%=")
      , InfixL (BiOp Mod <$ symbol "%")
      , InfixL (BiOp AsPow <$ symbol "**=") 
      , InfixL (BiOp Pow <$ symbol "**")    
      ]
    ,

    [   InfixL (BiOp AsMul <$ symbol "*=")
      ,	InfixL (BiOp Mul <$ symbol "*")
      , InfixL (BiOp AsDiv <$ symbol "/=")  
      , InfixL (BiOp Div <$ symbol "/") 
    
    ]
    ,

    [   InfixL (BiOp AsAdd <$ symbol "+=")
      , InfixL (BiOp AsSub <$ symbol "-=")
      ,	InfixL (BiOp Add <$ symbol "+")
      , InfixL (BiOp Sub <$ symbol "-")

      ]
    ,

      [ InfixN (BiOp Eq  <$ symbol "==")
      , InfixN (BiOp NotEq  <$ symbol "!=")
      ,	InfixN (BiOp LessEq  <$ symbol "<=")
      , InfixN (BiOp Less  <$ symbol "<" )
      , InfixN (BiOp GreaterEq  <$ symbol ">=") 
      , InfixN (BiOp Greater  <$ symbol ">" )  ]
    ,

      [ InfixL (BiOp And <$ rword  "and")
      , InfixL (BiOp Or  <$ rword  "or" ) ]

   	]

term :: Parser PTerm
term = parens pyExpr
  <|> bracers pyExpr
  <|> try (Var <$> identifier)
  <|> try (FloatConst <$> float) <|> try (IntConst <$> integer)
  <|> (BoolConst True <$ rword "true")
  <|> (BoolConst False <$ rword "false") 
  <|> Void <$ rword "Void"

{- rExpr :: Parser PTerm
rExpr = do
  a1 <- term
  op <- relation
  a2 <- term
  return (PTerm op a1 a2)

relation :: Parser Op
relation = (Greater <$ symbol ">") <|> (Less <$ symbol "<") <|> (Eq <$ symbol "==") -}

	





