module Gradualizer (
	readSystem,
	gradualize
) where

-- Type Systems
import InitialTypeSystem as ITS
import GradualTypeSystem as GTS

-- Parsers
import PrologParser
import SignatureParser

-- Gradulizer Steps
import Classify
import PatternMatch
import Flows
import LoneInputs
import Consistency

-- Imports
import Data.Maybe

-- read type system from prolog file,
-- parse contents and convert to TypeSystem,
-- then gradualize
readSystem :: FilePath -> IO GTS.TypeSystem
readSystem file = do
	-- parse prolog to Program format
	prolog <- parseProlog file
	-- parse signatures to Signatures format
	signatures <- parseSignatures file
	-- get type annotated terms
	let typeAnnotatedTerms = deriveTypeAnnotations signatures
	-- convert Program format to Initial Type System using annotated terms information
	let ts = ITS.toTypeSystem prolog typeAnnotatedTerms
	-- gradualize typesystem
	return $ gradualize signatures ts

-- gradualize type system by applying 6 steps:
-- - Step 1: Classify type variables with input or output modes
-- - Step 2: Classify type variables with producer or consumer positions
-- - Step 3: Apply pattern matching to constructed outputs
-- - Step 4: Flow and final type discovery
-- - Step 5: Restrict lone inputs to be static
-- - Step 6: Replace flow with consistency
-- - Add pattern matching rules
-- - Final: Remove mode and position from type variables and flow relation from typing relation
gradualize :: Signatures -> ITS.TypeSystem -> GTS.TypeSystem
gradualize signatures =
	-- final
	convertTypeSystem .
	-- add pattern match rules
	addPatternMatchRules signatures .
	-- step 6
	removeFlowsInsertConsistency .
	-- step 5
	loneInputsStatic .
	-- step 4
	insertFlowsFinalType .
	-- step 3
	applyPatternMatching .
	-- step 1 and 2
	classifyTypeVariables

-- PATTERN MATCH

-- add pattern matching rules to type system according to signatures
addPatternMatchRules :: Signatures -> ITS.TypeSystem -> ITS.TypeSystem
addPatternMatchRules (Signatures signatures) (ITS.TypeSystem typeRules) =
	-- first derive pattern matching rules for each type
	let patternMatchRules = concat $ map addPatternMatchRelation signatures
	-- add to type system
	in ITS.TypeSystem (typeRules ++ patternMatchRules)

-- Derive pattern matching rules for a given signatures
addPatternMatchRelation :: Signature -> [ITS.TypeRule]
addPatternMatchRelation (Signature name kind args)
	-- if signature corresponds to a Type
	| kind == "Type" =
		let
			-- build type with new variables
			typ = buildType name (length args)
			-- build type with dynamic type
			typDyn = buildTypeDyn name (length args)
			-- build pattern match rule for dynamic type
			dynRelation = ITS.TypeRule [] (ITS.MatchingRelation ITS.DynType typDyn)
			-- build pattern match rule for variable types
			typRelation = ITS.TypeRule [] (ITS.MatchingRelation typ typ)
		in [typRelation, dynRelation]
	| otherwise = []

-- build type, if is constructed instantiate with type variables
buildType :: String -> Int -> ITS.Type
buildType name 0 = ITS.BaseType name ITS.NullMode ITS.NullPosition
buildType "arrow" 2 = ITS.ArrowType (newVar "T1" "") (newVar "T2" "")
buildType "list" 1 = ITS.ListType (newVar "T" "")
buildType "pairType" 2 = ITS.PairType (newVar "T1" "") (newVar "T2" "")
buildType "refType" 1 = ITS.RefType (newVar "T" "")
buildType "sumType" 2 = ITS.SumType (newVar "T1" "") (newVar "T2" "")
buildType name numArgs = ITS.TypeConstructor name args
	where args = map (\x -> newVar ("T" ++ x) "") (map show [1..numArgs])

-- build type, if is constructed instantiate with dynamic type
buildTypeDyn :: String -> Int -> ITS.Type
buildTypeDyn name 0 = ITS.BaseType name ITS.NullMode ITS.NullPosition
buildTypeDyn "arrow" 2 = ITS.ArrowType ITS.DynType ITS.DynType
buildTypeDyn "list" 1 = ITS.ListType ITS.DynType
buildTypeDyn "pairType" 2 = ITS.PairType ITS.DynType ITS.DynType
buildTypeDyn "refType" 1 = ITS.RefType ITS.DynType
buildTypeDyn "sumType" 2 = ITS.SumType ITS.DynType ITS.DynType
buildTypeDyn name numArgs = ITS.TypeConstructor name args
	where args = replicate numArgs (ITS.DynType)

-- CONVERTION TO GRADUAL TYPE SYSTEM

convertTypeSystem :: ITS.TypeSystem -> GTS.TypeSystem
convertTypeSystem (ITS.TypeSystem ts) = GTS.TypeSystem $ map convertTypeRule ts

convertTypeRule :: ITS.TypeRule -> GTS.TypeRule
convertTypeRule (ITS.TypeRule premises conclusion) =
	GTS.TypeRule (convertRelation premises) (head $ convertRelation (conclusion:[]))

convertRelation :: [ITS.TypingRelation] -> [GTS.TypingRelation]
convertRelation [] = []
convertRelation ((ITS.TypeAssignment ctx expr typ) : trs) =
	GTS.TypeAssignment (convertContext ctx) (convertExpression expr) (convertType typ)
	: convertRelation trs
convertRelation ((ITS.MatchingRelation type1 type2) : trs) =
	GTS.MatchingRelation (convertType type1) (convertType type2)
	: convertRelation trs
convertRelation ((ITS.FlowRelation _ _) : trs) = convertRelation trs
convertRelation ((ITS.ConsistencyRelation type1 type2) : trs) =
	GTS.ConsistencyRelation (convertType type1) (convertType type2)
	: convertRelation trs
convertRelation ((ITS.StaticRelation typ) : trs) =
	GTS.StaticRelation (convertType typ)
	: convertRelation trs
convertRelation ((ITS.JoinRelation joinType types) : trs) =
	GTS.JoinRelation (convertType joinType) (map convertType types)
	: convertRelation trs
convertRelation ((ITS.SubtypingRelation type1 type2) : trs) =
	GTS.SubtypingRelation (convertType type1) (convertType type2)
	: convertRelation trs
convertRelation ((ITS.MemberRelation elem1 elem2) : trs) =
	GTS.MemberRelation (head $ convertContext [elem1]) (head $ convertContext [elem2])
	: convertRelation trs

convertContext :: ITS.Context -> GTS.Context
convertContext [] = []
convertContext ((ITS.Context var) : ctx) =
	GTS.Context var : convertContext ctx
convertContext ((ITS.Binding var typ) : ctx) =
	GTS.Binding var (convertType typ) : convertContext ctx

convertExpression :: ITS.Expression -> GTS.Expression
convertExpression (ITS.Var var) = GTS.Var var
convertExpression (ITS.Abstraction var expr) =
	GTS.Abstraction var (convertExpression expr)
convertExpression (ITS.Application expr1 expr2) =
	GTS.Application (convertExpression expr1) (convertExpression expr2)
convertExpression (ITS.Function name annotation exprs) =
	GTS.Function name (if isJust annotation
		then (Just $ convertType $ fromJust annotation)
		else Nothing) (map convertExpression exprs)

convertType :: ITS.Type -> GTS.Type
convertType (ITS.BaseType basetype mode position) =
	GTS.BaseType basetype
convertType (ITS.VarType name ident mode position)
	| ident == "" = GTS.VarType name
	| ident == "J" = GTS.VarType (name ++ "ᴶ")
	| otherwise = GTS.VarType (name ++ replicate (1 + read ident) '\'')
convertType (ITS.DynType) = GTS.DynType
convertType (ITS.ArrowType type1 type2) =
	GTS.ArrowType (convertType type1) (convertType type2)
convertType (ITS.ListType type1) =
	GTS.ListType (convertType type1)
convertType (ITS.PairType type1 type2) =
	GTS.PairType (convertType type1) (convertType type2)
convertType (ITS.RefType type1) =
	GTS.RefType (convertType type1)
convertType (ITS.SumType type1 type2) =
	GTS.SumType (convertType type1) (convertType type2)
convertType (ITS.TypeConstructor name types) =
	GTS.TypeConstructor name (map convertType types)
