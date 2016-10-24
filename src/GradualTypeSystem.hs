module GradualTypeSystem where

import Data.List
import Data.Maybe

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
data Bindings = Context String
			  | Binding String Type
			 deriving (Show, Eq, Ord)

-- Type annotations that may appear in expressions
type TypeAnnotation = Maybe Type

-- Expressions that can be formed in λ-calculus
data Expression = Var String
				| Abstraction String Expression
				| Application Expression Expression
				-- built in function
				| Function String TypeAnnotation [Expression]
				deriving (Show, Eq, Ord)

type Name = String

-- Types
data Type = BaseType Name
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
printExpression (Var var) = var
printExpression (Abstraction var e) =
	"(λ" ++ var ++ " . " ++ (printExpression e) ++ ")"
printExpression (Application e1 e2) = printExpression e1 ++ " " ++ printExpression e2
printExpression (Function ctr annotation exps) = ctr ++
	(if isJust annotation then "[" ++ printType (fromJust annotation) ++ "]" else "" ) ++ " " ++
	(concat $ intersperse " " $ map printExpression exps)

printRelation :: TypingRelation -> String
printRelation (TypeAssignment ctx expr typ) =
	--printContext ctx ++ " ⊢G " ++ printExpression expr ++ " : " ++ printType typ
	(concat $ intersperse ", " $ map printBindings ctx)
	++ " ⊢ " ++ printExpression expr ++ " : " ++ printType typ
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
