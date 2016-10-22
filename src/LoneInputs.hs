module LoneInputs (
	loneInputsStatic
) where

-- Type System
import InitialTypeSystem

-- Imports
import Control.Monad.State
import Data.List
import Data.Maybe

-- Step 5: Collect all type variables that only appear in input position
-- and add a Static typing relation for each

-- TODO:

-- Apply step 5 to a type system
loneInputsStatic :: TypeSystem -> TypeSystem
loneInputsStatic (TypeSystem typerules) = TypeSystem $ map step5TypeRules typerules

type VariableCollection = ([Type], [Type])

-- Apply step 5 to a type rule
step5TypeRules :: TypeRule -> TypeRule
step5TypeRules (TypeRule premises conclusion) =
	let
		-- get list of variables that appear only in input position
		(inputOnlyVariables, _) = snd $ runState (step5Relation $ premises ++ (conclusion : [])) ([],[])
		-- add static typing relations to premises
		premises' = premises ++ (map insertStatic inputOnlyVariables)
	in (TypeRule premises' conclusion)

-- collect input only type variables in typing relations
step5Relation :: [TypingRelation] -> State VariableCollection ()
step5Relation [] = return ()
step5Relation ((TypeAssignment ctx expr typ) : relations) = do
	mapM step5Context ctx
	step5Expression expr
	step5Type typ
	step5Relation relations
step5Relation ((MatchingRelation var typ) : relations) = do
	step5Type typ
	step5Relation relations
step5Relation (_ : relations) = do
	step5Relation relations

-- collect input only type variables in context
step5Context :: Bindings -> State VariableCollection ()
step5Context (Context _) = return ()
step5Context (Binding _ typ) = step5Type typ

-- collect input only type variables in expression
step5Expression :: Expression -> State VariableCollection ()
step5Expression (Var name) = return ()
step5Expression (Abstraction var expr) = do
	step5Expression expr
step5Expression (Application expr1 expr2) = do
	step5Expression expr1
	step5Expression expr2
step5Expression (Function name ta expr) = do
	mapM step5Expression expr
	if isJust ta then step5Type $ fromJust ta else return ()

-- check if variable has a output instance or only input instances
step5Type :: Type -> State VariableCollection ()
step5Type (VarType name ident mode position)
	-- if variable mode is output
 	| mode == Output = do
		-- get state
		(inputsOnlyVariables, outputsVariable) <- get
		-- check if variable already has been collected
		let outputsVariable' = if any (isEqualType var) outputsVariable
			-- if so do nothing
			then outputsVariable
			-- if not then add to list of output variables
			else insert var outputsVariable
		-- then remove said variable from inputsOnlyVariables
		let inputsOnlyVariables' = deleteBy isEqualType var inputsOnlyVariables
		put (inputsOnlyVariables', outputsVariable')
	-- if variable is input
	| mode == Input = do
		-- get state
		(inputsOnlyVariables, outputsVariable) <- get
		-- check if variable has output instance
		let inputsOnlyVariables'
			-- if so, don't add to inputsOnlyVariables
			| any (isEqualType var) outputsVariable = inputsOnlyVariables
			-- if the variable has only input instance,
			-- check if variable already is on inputs only variables list
			| otherwise = if any (isEqualType var) inputsOnlyVariables
				-- if so do nothing
				then inputsOnlyVariables
				-- else insert
				else insert var inputsOnlyVariables
		put (inputsOnlyVariables', outputsVariable)
	| otherwise = return ()
	where var = (VarType name ident mode position)
step5Type (ArrowType type1 type2) = do
	step5Type type1
	step5Type type2
step5Type (ListType typ) = do
	step5Type typ
step5Type (PairType type1 type2) = do
	step5Type type1
	step5Type type2
step5Type (RefType typ) = do
	step5Type typ
step5Type (SumType type1 type2) = do
	step5Type type1
	step5Type type2
step5Type (TypeConstructor name types) = do
	mapM step5Type types
	return ()
step5Type _ = do
	return ()

-- insert static typing relation given type
insertStatic :: Type -> TypingRelation
insertStatic typ = StaticRelation $ removeModePosition typ
