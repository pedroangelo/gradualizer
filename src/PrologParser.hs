module PrologParser (
	Program (..),
	Clause (..),
	Head (..),
	Body (..),
	Structure(..),
	Argument(..),
	parseProlog
) where

-- Imports
import Text.Parsec
import Text.Parsec.String
import Text.ParserCombinators.Parsec.Char
import Control.Applicative hiding((<|>), many, optional)

-- TODO:

-- Prolog Program is composed by clauses
data Program = Program [Import] [Clause]
	deriving (Show, Eq, Ord)

type Import = String

-- Clause is either a Fact or a Rule
data Clause
	= Fact Head
	| Rule Head Body
	deriving (Show, Eq, Ord)

type Head = Structure
type Body = [Structure]

-- Structure is a predicate call or predicate definition:
data Structure = Structure Name [Argument]
	deriving (Show, Eq, Ord)

-- Argument is a expression inside a structure
data Argument
	= Atom Name
	| Variable Name
	| Bind Name Name
	| List (Name, Name) Name
	| Predicate Name [Argument]
	deriving (Show, Eq, Ord)

type Name = String

parseProlog :: FilePath -> IO Program
parseProlog file = do
	contents <- readFile ("Type Systems in Prolog/" ++ file ++ ".yap")
	case parse programParser "" contents of
             { Left err -> error $ show err
             ; Right prolog -> return prolog
             }

-- Parser Functions

-- parse a atom (identifier starting with lower letter)
atomParser :: Parser Argument
atomParser = do
	name <- atom
	return $ Atom name

-- parse a variable (identifier starting with upper letter)
variableParser :: Parser Argument
variableParser = do
	name <- variable
	return $ Variable name

bindingParser :: Parser Argument
bindingParser = do
	name1 <- spaces *> (atom <|> variable)
	char ':'
	name2 <- spaces *> (atom <|> variable)
	return $ Bind name1 name2

-- parse a prolog list
-- whose elements are bindings of variables and types
-- <list> ::= [<name>:<name> | <name>]
listParser :: Parser Argument
listParser = do
	char '['
	name1 <- spaces *> (atom <|> variable) <* spaces
	char ':'
	name2 <- spaces *> (atom <|> variable) <* spaces
	char '|'
	name3 <- spaces *> (atom <|> variable) <* spaces
	char ']'
	return $ List (name1, name2) name3

-- parse a predicate call inside the body or head of a clause
-- <predicate> ::= <name>(<arguments>)
predicateParser :: Parser Argument
predicateParser = do
	-- a predicate starts with a atom
	name <- atom
	char '('
	-- followed by arguments (separeted by comma), encased in parethesis
	arguments <- (spaces *> argumentsParser <* spaces) `sepBy` comma
	char ')'
	return $ Predicate name arguments

-- a predicate call's argument can be:
-- a atom, list, atom or variable
argumentsParser :: Parser Argument
argumentsParser =
	(try bindingParser) <|>
	(try listParser) <|>
	(try predicateParser) <|>
	(try atomParser) <|>
	(try variableParser) <?> "arguments"

-- a structure is the relation definition if in the Head
-- or the premises of the relation if in the body
structureParser :: Parser Structure
structureParser = do
	name_ <- atom
	char '('
	arguments <- (spaces *> argumentsParser <* spaces) `sepBy` comma
	char ')'
	return $ Structure name_ arguments

-- parse a fact clause,
-- who is only composed by a structure that is the head
factParser :: Parser Clause
factParser = do
	head_ <- structureParser
	spaces *> char '.' <* spaces
	return $ Fact head_

-- parse a rule clause,
-- who is composed by a structure that is the head,
-- followed by several structures that form the body
ruleParser :: Parser Clause
ruleParser = do
	head_ <- structureParser
	spaces *> string ":-" <* spaces
	body <- (spaces *> structureParser <* spaces) `sepBy` (comma <* many newline)
	spaces *> char '.' <* spaces
	return $ Rule head_ body

-- parse a clause, that can be either a fact or a rule
clauseParser :: Parser Clause
clauseParser = try ruleParser <|> try factParser <?> "clause"

-- parse the prolog import statement
importParser :: Parser [Import]
importParser = do
	spaces *> string ":-" <* spaces
	spaces *> char '[' <* spaces
	imports <- atom `sepBy` (spaces *> comma <* spaces)
	spaces *> char ']' <* spaces
	char '.'
	return imports

-- parse program with import statement
programWithImportsParser :: Parser Program
programWithImportsParser = do
	optional commentsParser
	imports <- many newline *> importParser <* many newline
	clauses <- clauseParser `sepBy` many newline <* many newline
	return $ Program imports clauses

-- parse program without import statement
programWithoutImportsParser :: Parser Program
programWithoutImportsParser = do
	optional commentsParser
	clauses <- clauseParser `sepBy` many newline <* many newline
	return $ Program [] clauses

-- parse a prolog program
programParser :: Parser Program
programParser = try programWithImportsParser <|> programWithoutImportsParser

-- parse comments line, ignoring the contents of the comments
commentsParser :: Parser ()
commentsParser = do
	(many comments) `sepBy` newline <* many newline
	return ()

-- SIMPLE PARSERS

variable :: Parser Name
variable = (++) <$> (many1 upper) <*> many (alphaNum <|> char '_') <?> "variable"

atom :: Parser Name
atom = (++) <$> (many1 lower) <*> many (alphaNum <|> char '_') <?> "atom"

comma :: Parser Char
comma = char ',' <?> "comma"

comments :: Parser String
comments = do
	char '%'
	manyTill anyChar newline
