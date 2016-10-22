module PrologParser where

import Text.Parsec
import Text.Parsec.String
import Text.ParserCombinators.Parsec.Char
import Control.Applicative hiding((<|>), many, optional)

import InitialTypeSystem

-- TODO:
-- - read functions information (type annotations, etc) and pass using State Monad

-- Prolog Program is composed by clauses
data Program = Program [Import] [Clause]
	deriving (Show, Eq, Ord)

type Import = String
-- Clause is either a Fact or a Rule:
--	- <clause> ::= <fact>
--				 | <rule>
-- <fact> ::= <head>.
-- <rule> ::= <head> :- <body>.
-- <body> ::= <structure>
--			| <structure>, <structures>
data Clause = Fact Head
			| Rule Head Body
		   deriving (Show, Eq, Ord)

type Head = Structure
type Body = [Structure]

-- Structure is a predicate call or predicate definition:
--	- <structure> ::= <name>
--					| <name> (<arguments>)
-- <arguments> ::= <argument>
--				 | <argument> , <arguments>
data Structure = Structure Name [Argument]
	deriving (Show, Eq, Ord)

-- Argument is:
--	- <argument> ::= <name>
--				   | <list>
--				   | <predicate>
data Argument = Atom Name
			  | Variable Name
			  | List (Name, Name) Name
			  | Predicate Name [Argument]
			  deriving (Show, Eq, Ord)

--type Name = String


prologToTypeSystem :: FilePath -> IO TypeSystem
prologToTypeSystem file = do
	signatures <- readFile ("Type Systems in Prolog/" ++ file ++ ".sig")
	let function = lines signatures
	contents <- readFile ("Type Systems in Prolog/" ++ file ++ ".yap")
	case parse programParser "" contents of
             { Left err -> error $ show err
             ; Right prolog -> return $ toTypeSystem prolog function
             }

-- Convert from prolog to typesystem

toTypeSystem :: Program -> [String] -> TypeSystem
toTypeSystem (Program _ program) functions =
	TypeSystem $ map (\x -> toTypeRule x functions) program

toTypeRule :: Clause -> [String] -> TypeRule
toTypeRule (Fact head_) functions =
	TypeRule [] (toTypingRelation head_ functions)
toTypeRule (Rule head_ body) functions =
	TypeRule
		(map (\x -> toTypingRelation x functions) body)
		(toTypingRelation head_ functions)

toTypingRelation :: Structure -> [String] -> TypingRelation
toTypingRelation (Structure "type" arguments) functions = toTypeAssignment arguments functions
-- add for more typing relations

toTypeAssignment :: [Argument] -> [String] -> TypingRelation
toTypeAssignment (context:expression:type_:[]) functions =
	TypeAssignment (toContext context) (toExpression expression functions) (toType type_)

toContext :: Argument -> Context
toContext (Atom name) = undefined
toContext (Variable "Context") = [Context "Γ"]
toContext (Variable name) = undefined
toContext (List (var, type_) "Context") =
	[Context "Γ", Binding var (VarType type_ "" NullMode NullPosition)]
toContext (List (var, type_) tail_) =
	[Context tail_, Binding var (VarType type_ "" NullMode NullPosition)]
toContext (Predicate name args) = undefined

toExpression :: Argument -> [String] -> Expression
toExpression (Atom name) _ = Function name Nothing []
toExpression (Variable name) _ = Var name
toExpression (List (var, type_) tail_) _ = undefined
toExpression (Predicate "var" ((Variable name):[])) _ = Var name
toExpression (Predicate "abs" ((Variable var) : expr : [])) functions =
	Abstraction var (toExpression expr functions)
toExpression (Predicate "app" (expr1:expr2:[])) functions =
	Application (toExpression expr1 functions) (toExpression expr2 functions)
toExpression (Predicate name args) functions
	| elem name functions =
		let
			typeAnnotation = head args
			args' = tail args
		in Function name (Just $ toType typeAnnotation) (map (\x -> toExpression x functions) args')
	| otherwise = Function name Nothing (map (\x -> toExpression x functions) args)

toType :: Argument -> Type
toType (Atom name) = BaseType name NullMode NullPosition
toType (Variable name) = VarType name "" NullMode NullPosition
toType (List (var, type_) tail_) = undefined
toType (Predicate "arrow" (type1:type2:[])) = ArrowType (toType type1) (toType type2)
toType (Predicate "list" (type_:[])) = ListType (toType type_)
toType (Predicate "pairType" (type1:type2:[])) = PairType (toType type1) (toType type2)
toType (Predicate "refType" (type_:[])) = RefType (toType type_)
toType (Predicate "sumType" (type1:type2:[])) = SumType (toType type1) (toType type2)
toType (Predicate name args) = TypeConstructor name (map toType args)

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
	imports <- many (char '\n') *> importParser <* many (char '\n')
	clauses <- clauseParser `sepBy` many (char '\n') <* many (char '\n')
	return $ Program imports clauses

-- parse program without import statement
programWithoutImportsParser :: Parser Program
programWithoutImportsParser = do
	optional commentsParser
	clauses <- clauseParser `sepBy` many (char '\n') <* many (char '\n')
	return $ Program [] clauses

-- parse a prolog program
programParser :: Parser Program
programParser = try programWithImportsParser <|> programWithoutImportsParser

-- parse comments line, ignoring the contents of the comments
commentsParser :: Parser ()
commentsParser = do
	(many comments) `sepBy` (char '\n') <* many (char '\n')
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