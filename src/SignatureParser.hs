module SignatureParser (
	Signatures (..),
	Signature (..),
	parseSignatures,
	deriveTypeAnnotations
) where

-- Imports
import Text.Parsec
import Text.Parsec.String
import Text.ParserCombinators.Parsec.Char
import Control.Applicative hiding((<|>), many, optional)
import Data.Maybe

-- TODO:

data Signatures = Signatures [Signature]
	deriving Show

data Signature = Signature Name Kind [Kind]
	deriving Show

type Name = String
type Kind = String

parseSignatures :: FilePath -> IO Signatures
parseSignatures file = do
	contents <- readFile ("Type Systems in Prolog/" ++ file ++ ".hs")
	let signatures = lines contents
	case mapM (parse signatureParser "") signatures of
             { Left err -> error $ show err
             ; Right sig -> return $ Signatures sig
             }

deriveTypeAnnotations :: Signatures -> [String]
deriveTypeAnnotations (Signatures signatures) = filter (/= "") $ map isTypeAnnotated signatures

isTypeAnnotated :: Signature -> String
isTypeAnnotated (Signature name kind args)
	| length args == 0 = ""
	| kind == "Term" && (head args) == "Type" = name
	| otherwise = ""

-- Parser Functions

signatureParser :: Parser Signature
signatureParser = do
	name_ <- name
	string " :: "
	arguments  <- kind `sepBy` (string " -> ")
	return $ Signature name_ (last arguments) (init arguments)

signaturesParser :: Parser Signatures
signaturesParser = do
	signatures <- signatureParser `sepBy` newline <* newline
	return $ Signatures signatures

-- SIMPLE PARSERS

kind :: Parser Kind
kind = (++) <$> (many1 upper) <*> many alphaNum <?> "kind"

name :: Parser Name
name = (++) <$> (many1 lower) <*> many alphaNum <?> "name"
