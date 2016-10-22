module Classify (
	classifyTypeVariables
) where

-- Type System
import InitialTypeSystem

-- Imports
import Data.Maybe

-- Step 1: Classify type variables with input or output modes
-- Step 2: Classsify type variables with producer or consumer positions

-- TODO:
-- - check if type constructor is covariant or contravariant

-- classify type system
classifyTypeVariables :: TypeSystem -> TypeSystem
classifyTypeVariables (TypeSystem typerules) = TypeSystem $ map step12TypeRule typerules

-- classify type rule
step12TypeRule :: TypeRule -> TypeRule
step12TypeRule (TypeRule premises conclusion) = TypeRule (map step12Premise premises) (step12Conclusion conclusion)

-- classify premise, modes and positions are not flipped
step12Premise :: TypingRelation -> TypingRelation
step12Premise (TypeAssignment ctx expr typ) =
	TypeAssignment
		-- type variables in context are normally considered inputs and consumers
		(map (\x -> step12Bindings x Input Consumer) ctx)
		-- type variables in expression are normally considered inputs and consumers
		(step12Expression expr Input Consumer)
		-- type variables in type are normally considered output and producers
		(step12Type typ Output Producer)

-- classify conclusion, modes and positions are flipped
step12Conclusion :: TypingRelation -> TypingRelation
step12Conclusion (TypeAssignment ctx expr typ) =
	TypeAssignment
		-- type variables in context are output producers
		(map (\x -> step12Bindings x Output Producer) ctx)
		-- type variables in expression are output producers
		(step12Expression expr Output Producer)
		-- type variables in type are input consumers
		(step12Type typ Input Consumer)

-- change mode and position for type variables in context
step12Bindings :: Bindings -> Mode -> Position -> Bindings
step12Bindings (Context name) _ _ = Context name
step12Bindings (Binding name typ) mode position = Binding name $ step12Type typ mode position

-- change mode and position for type variables in expression
step12Expression :: Expression -> Mode -> Position -> Expression
step12Expression (Var var) _ _ = Var var
step12Expression (Abstraction var expression) mode position =
	Abstraction var (step12Expression expression mode position)
step12Expression (Application expr1 expr2) mode position =
	Application (step12Expression expr1 mode position) (step12Expression expr2 mode position)
step12Expression (Function name typeannotation exprs) mode position =
	Function name
		(if isJust typeannotation then Just (step12Type (fromJust typeannotation) mode position) else Nothing)
		(map (\x -> step12Expression x mode position) exprs)

-- change mode and position for type variables
step12Type :: Type -> Mode -> Position -> Type
-- most types are straightforward
step12Type (BaseType name _ _) mode position = BaseType name mode position
step12Type (VarType name ident _ _) mode position = VarType name ident mode position
step12Type (DynType) _ _ = DynType
-- however some type constructors, like arrow, are contravariant in the first argument,
-- therefore the position is flipped
step12Type (ArrowType type1 type2) mode position =
	ArrowType (step12Type type1 mode (changePosition position)) (step12Type type2 mode position)
step12Type (ListType typ) mode position =
	ListType (step12Type typ mode position)
step12Type (PairType type1 type2) mode position =
	PairType (step12Type type1 mode position) (step12Type type2 mode position)
step12Type (RefType typ) mode position =
	RefType (step12Type typ mode position)
step12Type (SumType type1 type2) mode position =
	SumType (step12Type type1 mode position) (step12Type type2 mode position)
-- in case of the type constructor, we must check if it is covariant or contravariant
step12Type (TypeConstructor name types) mode position =
	TypeConstructor name (map (\x -> step12Type x mode position) types)
