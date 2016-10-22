module Gradualizer where

import InitialTypeSystem as ITS
import GradualTypeSystem as GTS
import PrologParser
import Classify
import PatternMatch
import Flows
import LoneInputs
import Consistency

import Data.Maybe


readSystem :: FilePath -> IO GTS.TypeSystem
readSystem file = do
	ts <- prologToTypeSystem file
	return $ gradualize $ ts

gradualize :: ITS.TypeSystem -> GTS.TypeSystem
gradualize =
	convertTypeSystem .
	removeFlowsInsertConsistency .
	loneInputsStatic .
	insertFlowsFinalType .
	applyPatternMatching .
	classifyTypeVariables

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
