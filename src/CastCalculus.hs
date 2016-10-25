module CastCalculus where

import Data.List
import Data.Maybe

import qualified InitialTypeSystem as ITS

-- Type system is composed of type rules
data TypeSystem = TypeSystem [TypeRule]
	deriving (Show, Eq, Ord)

-- Type rule is a collection of premises with one conclusion
data TypeRule = TypeRule Premises Conclusion
	deriving (Show, Eq, Ord)

type Premises = [TypingRelation]
type Conclusion = TypingRelation

-- Typing relation can be:
data TypingRelation
	-- Type Assignment: (Context, Expression, Type)
	= TypeAssignment Context Expression Type
	-- Pattern Matching: Type ▷ Type
	| MatchingRelation Type Type
	-- Consistency Relation : Type ~ Type
	| ConsistencyRelation Type Type
	-- Static Relation: static(Type)
	| StaticRelation Type
	-- Join Relation: Type = ⊔ [Type]
	| JoinRelation Type [Type]
	-- Subtyping Relation: Type <: Type
	| SubtypingRelation Type Type
	-- Member Relation: (Name:Type) ∈ Context
	| MemberRelation Bindings Bindings
	deriving (Show, Eq, Ord)

-- Context holds bindings between variables and types
type Context = [Bindings]
data Bindings
	= Context String
	| Binding String Type
	deriving (Show, Eq, Ord)

-- Type annotations that may appear in expressions
type TypeAnnotation = Maybe Type

-- Expressions that can be formed in λ-calculus
data Expression
	= Var String Cast
	| Abstraction String Expression
	| Application Expression Expression
	| Function String TypeAnnotation [Expression]
	| Compilation Expression Expression
	deriving (Show, Eq, Ord)

type Cast = Maybe (Type, Type)

type Name = String

-- Types
data Type
	= BaseType Name
	| VarType Name
	| DynType
	| ArrowType Type Type
	| ListType Type
	| PairType Type Type
	| RefType Type
	| SumType Type Type
	| TypeConstructor Name [Type]
	deriving (Show, Eq, Ord)

-- PRINTING

printContext :: Context -> String
printContext [] = ""
printContext ((Context var): ctx) = var ++ "" ++ printContext ctx
printContext ((Binding var typ): ctx) = ", " ++ var ++ " : " ++ printType typ ++ printContext ctx

printBindings :: Bindings -> String
printBindings(Context var) = var
printBindings (Binding var typ) = var ++ " : " ++ printType typ

printType :: Type -> String
printType (BaseType basetype) = basetype
printType (VarType name) = name
printType (DynType) = "*"
printType (ArrowType type1 type2) = printType type1 ++ " -> " ++ printType type2
printType (ListType type1) = "list " ++ printType type1
printType (PairType type1 type2) = "pairType " ++ printType type1 ++ " " ++ printType type2
printType (RefType type1) = "refType " ++ printType type1
printType (SumType type1 type2) = "sumType " ++ printType type1 ++ " " ++ printType type2
printType (TypeConstructor tc typ) = tc ++ " " ++ (concat $ intersperse " " (map printType typ))

printExpression :: Expression -> String
printExpression (Var var cast)
	| cast == Nothing = var
	| otherwise = "(" ++ var ++ printCast cast ++ ")"
printExpression (Abstraction var e) =
	"(λ" ++ var ++ " . " ++ (printExpression e) ++ ")"
printExpression (Application e1 e2) = printExpression e1 ++ " " ++ printExpression e2
printExpression (Function ctr annotation exps) = ctr ++
	(if isJust annotation then "[" ++ printType (fromJust annotation) ++ "]" else "" ) ++ " " ++
	(concat $ intersperse " " $ map printExpression exps)
printExpression (Compilation expr1 expr2) =
	printExpression expr1 ++ " ⇝ " ++ printExpression expr2

printCast :: Cast -> String
printCast (Nothing) = ""
printCast (Just (type1, type2)) = " : " ++ printType type1 ++ " => " ++ printType type2

printRelation :: TypingRelation -> String
printRelation (TypeAssignment ctx expr typ) =
	--printContext ctx ++ " ⊢G " ++ printExpression expr ++ " : " ++ printType typ
	(concat $ intersperse ", " $ map printBindings ctx)
	++ " ⊢CC " ++ printExpression expr ++ " : " ++ printType typ
printRelation (MatchingRelation type1 type2) = printType type1 ++ " ▷ " ++ printType type2
printRelation (ConsistencyRelation type1 type2) = printType type1 ++ " ~ " ++ printType type2
printRelation (StaticRelation type1) = "static(" ++ printType type1 ++ ")"
printRelation (JoinRelation typeJ types) =
	printType typeJ ++ " = " ++ concat (intersperse " ⊔ " (map printType types))
printRelation (SubtypingRelation type1 type2) = printType type1 ++ " <: " ++ printType type2
printRelation (MemberRelation element set) = printBindings element ++ " ∈ " ++ printBindings set

printRule :: TypeRule -> String
printRule (TypeRule premise conclusion) =
	(concat $ intersperse "\n" $ map printRelation premise) ++
	(if null premise then "" else "\n" ++ "=>"  ++ "\n") ++ printRelation conclusion

printSystem' :: TypeSystem -> String
printSystem' (TypeSystem ts) = concat $ intersperse "\n\n" $ map printRule ts

printSystem ts = putStrLn $ printSystem' ts

-- CONVERTION TO CAST CALCULUS TYPE SYSTEM

convertTypeSystem :: ITS.TypeSystem -> TypeSystem
convertTypeSystem (ITS.TypeSystem ts) = TypeSystem $ map convertTypeRule ts

convertTypeRule :: ITS.TypeRule -> TypeRule
convertTypeRule (ITS.TypeRule premises conclusion) =
	TypeRule (convertRelation premises) (head $ convertRelation (conclusion:[]))

convertRelation :: [ITS.TypingRelation] -> [TypingRelation]
convertRelation [] = []
convertRelation ((ITS.TypeAssignment ctx expr typ) : trs) =
	TypeAssignment (convertContext ctx) (convertExpression expr) (convertType typ)
	: convertRelation trs
convertRelation ((ITS.MatchingRelation type1 type2) : trs) =
	MatchingRelation (convertType type1) (convertType type2)
	: convertRelation trs
convertRelation ((ITS.FlowRelation _ _) : trs) = convertRelation trs
convertRelation ((ITS.ConsistencyRelation type1 type2) : trs) =
	ConsistencyRelation (convertType type1) (convertType type2)
	: convertRelation trs
convertRelation ((ITS.StaticRelation typ) : trs) =
	StaticRelation (convertType typ)
	: convertRelation trs
convertRelation ((ITS.JoinRelation joinType types) : trs) =
	JoinRelation (convertType joinType) (map convertType types)
	: convertRelation trs
convertRelation ((ITS.SubtypingRelation type1 type2) : trs) =
	SubtypingRelation (convertType type1) (convertType type2)
	: convertRelation trs
convertRelation ((ITS.MemberRelation elem1 elem2) : trs) =
	MemberRelation (head $ convertContext [elem1]) (head $ convertContext [elem2])
	: convertRelation trs

convertContext :: ITS.Context -> Context
convertContext [] = []
convertContext ((ITS.Context var) : ctx) =
	Context var : convertContext ctx
convertContext ((ITS.Binding var typ) : ctx) =
	Binding var (convertType typ) : convertContext ctx

convertExpression :: ITS.Expression -> Expression
convertExpression (ITS.Var var cast) = Var var (convertCast cast)
convertExpression (ITS.Abstraction var expr) =
	Abstraction var (convertExpression expr)
convertExpression (ITS.Application expr1 expr2) =
	Application (convertExpression expr1) (convertExpression expr2)
convertExpression (ITS.Function name annotation exprs) =
	Function name (if isJust annotation
		then (Just $ convertType $ fromJust annotation)
		else Nothing) (map convertExpression exprs)
convertExpression (ITS.Compilation expr1 expr2) =
	Compilation (convertExpression expr1) (convertExpression expr2)

convertCast :: ITS.Cast -> Cast
convertCast (Nothing) = Nothing
convertCast (Just (type1, type2)) = Just (convertType type1, convertType type2)

convertType :: ITS.Type -> Type
convertType (ITS.BaseType basetype mode position) =
	BaseType basetype
convertType (ITS.VarType name ident mode position)
	| ident == "" = VarType name
	| ident == "J" = VarType (name ++ "ᴶ")
	| otherwise = VarType (name ++ replicate (1 + read ident) '\'')
convertType (ITS.DynType) = DynType
convertType (ITS.ArrowType type1 type2) =
	ArrowType (convertType type1) (convertType type2)
convertType (ITS.ListType type1) =
	ListType (convertType type1)
convertType (ITS.PairType type1 type2) =
	PairType (convertType type1) (convertType type2)
convertType (ITS.RefType type1) =
	RefType (convertType type1)
convertType (ITS.SumType type1 type2) =
	SumType (convertType type1) (convertType type2)
convertType (ITS.TypeConstructor name types) =
	TypeConstructor name (map convertType types)
