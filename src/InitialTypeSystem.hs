module InitialTypeSystem where

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
-- 		Type Assignment: (Context, Expression, Type)
data TypingRelation = TypeAssignment Context Expression Type
					-- Pattern Matching: Type ▷ Type
					| MatchingRelation Type Type
					-- Flow Relation: Type ⇝ Type
					| FlowRelation Type Type
					-- Consistency Relation : Type ~ Type
					| ConsistencyRelation Type Type
					-- Static Relation: static(Type)
					| StaticRelation Type
					-- Join Relation: Type = ⊔ [Type]
					| JoinRelation Type [Type]
					-- Subtyping Relation: Type <: Type
					| SubtypingRelation Type Type
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
type Identifier = String

-- Types
data Type = BaseType Name Mode Position
		  | VarType Name Identifier Mode Position
		  | DynType
		  | ArrowType Type Type
		  | ListType Type
		  | PairType Type Type
		  | RefType Type
		  | SumType Type Type
		  | TypeConstructor Name [Type]
		  deriving (Show, Eq, Ord)

-- Mode used during gradualizer steps
data Mode = NullMode
		  | Input
		  | Output
		  deriving (Show, Eq, Ord)

-- Position used during gradualizer steps
data Position = NullPosition
			  | Producer
			  | Consumer
			  deriving (Show, Eq, Ord)

-- PRINTING

printContext :: Context -> String
printContext [] = ""
printContext ((Context var) : ctx) = var ++ "" ++ printContext ctx
printContext ((Binding var typ) : ctx) = ", " ++ var ++ " : " ++ printType typ ++ printContext ctx

printType :: Type -> String
printType (BaseType basetype mode position) = basetype ++ printMode mode ++ printPosition position
printType (VarType name ident mode position) = name ++ "_" ++ ident ++ printMode mode ++ printPosition position
printType (DynType) = "*"
printType (ArrowType type1 type2) = printType type1 ++ " -> " ++ printType type2
printType (ListType type1) = "list " ++ printType type1
printType (PairType type1 type2) = "pairType " ++ printType type1 ++ " " ++ printType type2
printType (RefType type1) = "refType " ++ printType type1
printType (SumType type1 type2) = "sumType " ++ printType type1 ++ " " ++ printType type2
printType (TypeConstructor tc typ) = tc ++ "[" ++ (concat $ intersperse ", " (map printType typ)) ++ "]"

printMode :: Mode -> String
printMode NullMode = ""
printMode Input = "I"
printMode Output = "O"

printPosition :: Position -> String
printPosition NullPosition = ""
printPosition Producer = "P"
printPosition Consumer = "C"

printExpression :: Expression -> String
printExpression (Var var) = var
printExpression (Abstraction var expr) =
	"(λ" ++ var ++ " . " ++ (printExpression expr) ++ ")"
printExpression (Application expr1 expr2) = printExpression expr1 ++ " " ++ printExpression expr2
printExpression (Function name annotation exps) = name ++
	(if isJust annotation then "[" ++ printType (fromJust annotation) ++ "]" else "" ) ++ " " ++
	(concat $ intersperse " " $ map printExpression exps)

printRelation :: TypingRelation -> String
printRelation (TypeAssignment ctx expr typ) =
	printContext ctx ++ " ⊢ " ++ printExpression expr ++ " : " ++ printType typ
printRelation (MatchingRelation type1 type2) = printType type1 ++ " ▷ " ++ printType type2
printRelation (FlowRelation type1 type2) = printType type1 ++ " ⇝ " ++ printType type2
printRelation (ConsistencyRelation type1 type2) = printType type1 ++ " ~ " ++ printType type2
printRelation (StaticRelation type1) = "static(" ++ printType type1 ++ ")"
printRelation (JoinRelation typeJ types) =
	printType typeJ ++ " = " ++ concat (intersperse " ⊔ " (map printType types))
printRelation (SubtypingRelation type1 type2) = printType type1 ++ " <: " ++ printType type2

printRule :: TypeRule -> String
printRule (TypeRule premise conclusion) =
	(concat $ intersperse "\n" $ map printRelation premise) ++
	(if null premise then "" else "\n" ++ "V"  ++ "\n") ++ printRelation conclusion

printSystem' :: TypeSystem -> String
printSystem' (TypeSystem ts) = concat $ intersperse "\n\n" $ map printRule ts

printSystem ts = putStrLn $ printSystem' ts

-- HELPER FUNCTIONS

-- switch position marking
changePosition :: Position -> Position
changePosition NullPosition = NullPosition
changePosition Producer = Consumer
changePosition Consumer = Producer

pm :: Int -> Type
pm i = newVar ("PM" ++ show i) ""

-- build new variable
newVar :: String -> String -> Type
newVar name ident = VarType name ident NullMode NullPosition

isOutputType :: Type -> Bool
isOutputType (BaseType _ Output _) = True
isOutputType (VarType _ _ Output _) = True
isOutputType (ArrowType type1 type2) = isOutputType type1 && isOutputType type2
isOutputType (ListType type_) = isOutputType type_
isOutputType (PairType type1 type2) = isOutputType type1 && isOutputType type2
isOutputType (RefType type_) = isOutputType type_
isOutputType (SumType type1 type2) = isOutputType type1 && isOutputType type2
isOutputType (TypeConstructor _ types) = all isOutputType types
isOutputType _ = False

-- get name of type
typeName :: Type -> String
typeName (BaseType name _ _) = name
typeName (VarType name _ _ _) = name
typeName (TypeConstructor name _) = name
typeName _ = ""

typeIdent :: Type -> String
typeIdent (VarType _ ident _ _) = ident
typeIdent _ = ""

-- check if variable is in input mode
isInput :: Type -> Bool
isInput (VarType _ _ Input _) = True
isInput _ = False

-- check if variable is in output mode
isOutput :: Type -> Bool
isOutput (VarType _ _ Output _) = True
isOutput _ = False

-- check if variable is in consumer position
isConsumer :: Type -> Bool
isConsumer (VarType _ _ _ Consumer) = True
isConsumer _ = False

-- check if variable is in producer position
isProducer :: Type -> Bool
isProducer (VarType _ _ _ Producer) = True
isProducer _ = False

-- check if variable is a join type
isJoinType :: Type -> Bool
isJoinType (VarType _ ident _ _) = ident == "J"
isJoinType _ = False

-- check if variable has same name as string
isEqualVar :: String -> Type -> Bool
isEqualVar var (VarType name ident _ _) = var == name
isEqualVar _ _ = False

-- check if type variable is the same, despite mode and position
isEqualType :: Type -> Type -> Bool
isEqualType (BaseType name1 _ _) (BaseType name2 _ _) =
	name1 == name2
isEqualType (VarType name1 ident1 _ _) (VarType name2 ident2 _ _) =
	name1 == name2 && ident1 == ident2
isEqualType (DynType) (DynType) = True
isEqualType (ArrowType type1a type1b) (ArrowType type2a type2b) =
	isEqualType type1a type2a && isEqualType type1b type2b
isEqualType (ListType type1) (ListType type2) =
	isEqualType type1 type2
isEqualType (PairType type1a type1b) (PairType type2a type2b) =
	isEqualType type1a type2a && isEqualType type1b type2b
isEqualType (RefType type1) (RefType type2) =
	isEqualType type1 type2
isEqualType (SumType type1a type1b) (SumType type2a type2b) =
	isEqualType type1a type2a && isEqualType type1b type2b
isEqualType (TypeConstructor name1 types1) (TypeConstructor name2 types2) =
	name1 == name2 && (all (== True) (zipWith (==) types1 types2))

-- remove mode and position tags from types
removeModePosition :: Type -> Type
removeModePosition (BaseType name _ _) = BaseType name NullMode NullPosition
removeModePosition (VarType name ident _ _) = VarType name ident NullMode NullPosition
removeModePosition (DynType) = DynType
removeModePosition (ArrowType type1 type2) =
	ArrowType (removeModePosition type1) (removeModePosition type2)
removeModePosition (ListType typ) = ListType $ removeModePosition typ
removeModePosition (PairType type1 type2) =
	PairType (removeModePosition type1) (removeModePosition type2)
removeModePosition (RefType typ) = RefType $ removeModePosition typ
removeModePosition (SumType type1 type2) =
	SumType (removeModePosition type1) (removeModePosition type2)
removeModePosition (TypeConstructor name types) =
	TypeConstructor name $ map removeModePosition types
