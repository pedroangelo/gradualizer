module Flows (
	insertFlowsFinalType
) where

-- Type System
import InitialTypeSystem

-- Imports
import Control.Monad.State
import Data.Maybe

-- Step 4: Replace repeating output type variables with fresh variables,
-- find final type, mark each producer flowing to consumer through
-- the final type, and finally replace each input with the final type

-- TODO:
-- - collect repeating variables for diferent typing relations (step4_1TypingRelation)
-- - change names of variables and collect final type information on other typing relations (step4_2TypingRelation)
-- - replace input variable with final types for other typing relations (replaceInputVariable)

-- Apply step 4 to a type system
insertFlowsFinalType :: TypeSystem -> TypeSystem
insertFlowsFinalType (TypeSystem typerules) = TypeSystem $ map step4TypeRule typerules

-- Apply step 4 to a type rule
step4TypeRule :: TypeRule -> TypeRule
step4TypeRule typerule =
	let
		-- first find all type variables along with the number of repetitions
		(_, variables) = runState (collectOutputVariables typerule) []
		-- filter through type variables that occur 2 or more times
		variables' = map fst $ filter ((>=2) . snd) variables
	-- apply step 4 to the typerule for each type variables
	-- switch variable parameters to conform with foldr
	in foldr (\x y -> step4Variable y x) typerule variables'

-- apply step 4 to a type rule according to a single type variables
step4Variable :: TypeRule -> String -> TypeRule
step4Variable typerule targetVar =
	let
		-- change all repeating type variables, renaming them with new names
		-- also obtain the collection of all type variables (step 4.1)
		result = runState (changeCollectVariables typerule) (0, targetVar, Nothing, [])
		(TypeRule premises conclusion, varInfo) = result
		-- find final type for each variable and insert flows (step 4.2, 4.3)
		(finalType, flowsJoins) = findFinalTypeInsertFlows varInfo
		-- replace input variables with final type (step 4.4)
		premises' = map (\x -> replaceInputVariable x targetVar finalType) premises
		conclusion' = replaceInputVariable conclusion targetVar finalType
	in TypeRule (premises' ++ flowsJoins) conclusion'


-- Step 4.1 - Collect all repeating type variables along with the number of repetitions
-- only output type variables can be collected

-- information on repeating type variables
type VariableCollection = [(String, Int)]

-- collect all repeating type variables in a type rule
collectOutputVariables :: TypeRule -> State VariableCollection ()
collectOutputVariables (TypeRule premises conclusion) = do
	step4_1TypingRelation premises
	step4_1TypingRelation $ conclusion : []

-- collect all repeating type variables in a typing relations
-- collection happens uniformly, despite diferent typing relations
step4_1TypingRelation :: [TypingRelation] -> State VariableCollection ()
step4_1TypingRelation [] = return ()
step4_1TypingRelation ((TypeAssignment ctx expr typ) : premises) = do
	mapM step4_1Context ctx
	step4_1Expression expr
	step4_1Type typ
	step4_1TypingRelation premises
step4_1TypingRelation ((MatchingRelation var typ) : premises) = do
	step4_1Type typ
	step4_1TypingRelation premises
step4_1TypingRelation (_ : premises) = do
	step4_1TypingRelation premises

-- collect repeating type variables in context
step4_1Context :: Bindings -> State VariableCollection ()
step4_1Context (Context _) = return ()
step4_1Context (Binding _ typ) = step4_1Type typ

-- collect repeating type variables in expression
step4_1Expression :: Expression -> State VariableCollection ()
step4_1Expression (Var name cast) = return ()
step4_1Expression (Abstraction var expr) = do
	step4_1Expression expr
step4_1Expression (Application expr1 expr2) = do
	step4_1Expression expr1
	step4_1Expression expr2
step4_1Expression (Function name ta expr) = do
	mapM step4_1Expression expr
	if isJust ta then step4_1Type $ fromJust ta else return ()

-- collect type variables
step4_1Type :: Type -> State VariableCollection ()
step4_1Type (VarType name ident Output position) = do
	collection <- get
	-- if variable already exists in state
	if (elem name $ map fst collection)
		-- increment counter
		then put $ map (insertVar name) collection
		-- if not add to state
		else put $ collection ++ [(name, 1)]
step4_1Type (ArrowType type1 type2) = do
	step4_1Type type1
	step4_1Type type2
step4_1Type (ListType typ) = do
	step4_1Type typ
step4_1Type (PairType type1 type2) = do
	step4_1Type type1
	step4_1Type type2
step4_1Type (RefType typ) = do
	step4_1Type typ
step4_1Type (SumType type1 type2) = do
	step4_1Type type1
	step4_1Type type2
step4_1Type (TypeConstructor name types) = do
	mapM step4_1Type types
	return ()
step4_1Type _ = do
	return ()

-- increment counter for given variable
insertVar :: String -> (String, Int) -> (String, Int)
insertVar name (var, i)
	| name == var = (var, i+1)
	| otherwise = (var, i)

-- Step 4.2 - Change names of type variables and collect information on final type

-- Information on final type
type Counter = Int
type TargetVariable = String
type VariableInfo = (Counter, TargetVariable, TypeAnnotation, [Type])

-- apply step 4.2 in type rule
changeCollectVariables :: TypeRule -> State VariableInfo TypeRule
changeCollectVariables (TypeRule premises conclusion) = do
	premises' <- step4_2TypingRelation premises
	conclusion' <- step4_2TypingRelation $ conclusion : []
	return $ TypeRule premises' $ head conclusion'

-- apply step 4.2 in typing relations
step4_2TypingRelation :: [TypingRelation] -> State VariableInfo [TypingRelation]
step4_2TypingRelation [] = return []
-- if is type assignment
step4_2TypingRelation ((TypeAssignment ctx expr typ) : premises) = do
	-- change type variables in context
	ctx' <- mapM step4_2Context ctx
	-- change type variables in expression
	expr' <- step4_2Expression expr
	-- change type variables
	typ' <- step4_2Type typ
	-- apply step 4.2 in the rest of the typing relations
	premises' <- step4_2TypingRelation premises
	return $ (TypeAssignment ctx' expr' typ') : premises'
-- if is matching relation
step4_2TypingRelation ((MatchingRelation var typ) : premises) = do
	-- change type variables
	typ' <- step4_2Type typ
	-- apply step 4.2 in the rest of the typing relations
	premises' <- step4_2TypingRelation premises
	return $ (MatchingRelation var typ') : premises'
-- apply step 4.2 to other typing relations
step4_2TypingRelation (anything : premises) = do
	premises' <- step4_2TypingRelation premises
	return $ anything : premises'

-- apply step 4.2 to context
step4_2Context :: Bindings -> State VariableInfo Bindings
step4_2Context (Context name) = return (Context name)
step4_2Context (Binding var typ) = do
	typ' <- step4_2Type typ
	return $ (Binding var typ')

-- apply step 4.2 to expression
step4_2Expression :: Expression -> State VariableInfo Expression
step4_2Expression (Var name cast) = return (Var name cast)
step4_2Expression (Abstraction var expr) = do
	expr' <- step4_2Expression expr
	return (Abstraction var expr')
step4_2Expression (Application expr1 expr2) = do
	expr1' <- step4_2Expression expr1
	expr2' <- step4_2Expression expr2
	return (Application expr1' expr2')
-- in case if a built in function
step4_2Expression (Function name typeannotation expr)
	-- check if has type anotations
	| isJust typeannotation = do
		-- get final type information
		(i, targetVar, _, variables) <- get
		-- put typeAnnotation type in type information
		put (i, targetVar, typeannotation, variables)
		expr' <- mapM step4_2Expression expr
		return $ (Function name typeannotation expr')
	| otherwise = do
		expr' <- mapM step4_2Expression expr
		return $ (Function name typeannotation expr')

-- apply step 4.2 to type
step4_2Type :: Type -> State VariableInfo Type
step4_2Type (VarType name ident mode position) = do
	(i, targetVar, ta, variables) <- get
	-- only collect repeating type variables for final type information
	if targetVar == name
		then do
			-- change type variable name
			let typ' = VarType name (show i) mode position
			-- put type variable in final type information and increment counter
			put (i+1, targetVar, ta, variables ++ [typ'])
			return typ'
		-- if type variable doesn't have the same name, no need in collecting them
		else return (VarType name ident mode position)
step4_2Type (ArrowType type1 type2) = do
	type1' <- step4_2Type type1
	type2' <- step4_2Type type2
	return (ArrowType type1' type2')
step4_2Type (ListType typ) = do
	typ' <- step4_2Type typ
	return (ListType typ')
step4_2Type (PairType type1 type2) = do
	type1' <- step4_2Type type1
	type2' <- step4_2Type type2
	return (PairType type1' type2')
step4_2Type (RefType typ) = do
	typ' <- step4_2Type typ
	return (RefType typ')
step4_2Type (SumType type1 type2) = do
	type1' <- step4_2Type type1
	type2' <- step4_2Type type2
	return (SumType type1' type2')
step4_2Type (TypeConstructor name types) = do
	types' <- mapM step4_2Type types
	return (TypeConstructor name types')
step4_2Type typ = return typ

-- Step 4.3 - Find final type and insert flows

-- final type and new flows
type FinalType = Type
type FlowsJoins = [TypingRelation]

-- Find the final type and insert flows to type rule
findFinalTypeInsertFlows :: VariableInfo -> (FinalType, FlowsJoins)
findFinalTypeInsertFlows (counter, targetVar, typeannotation, types)
	-- if exists type annotation, then final type is type annotation
	| isJust typeannotation =
		let
			finaltype = fromJust typeannotation
			removeInputs = filter isOutput types
		-- build flows to and from final type
		in (finaltype, flows finaltype removeInputs)
	-- if any of the types is output consumer, then he is final type
	| any (\x -> isOutput x && isConsumer x) types =
		let
			-- final type is output consumer
			finaltype = head $ filter (\x -> isOutput x && isConsumer x) types
			-- remove other type variables, already removed
			--removeOtherVar = filter (not . isEqualVar targetVar) types
			-- remove output consumer, no need to flow to himself
			removeInputs = filter isOutput types
			removeOutputConsumer = filter (not . (\x -> isOutput x && isConsumer x)) removeInputs
		in (finaltype, flows finaltype removeOutputConsumer)
	| otherwise = let
		-- final type is join of producers
		finaltype = newVar targetVar "J"
		--removeOtherVar = filter (not . isEqualVar targetVar) types
		--removeOutputConsumers = filter (not . isOutputConsumer) types
		-- insert flows from and to final type
		removeConsumer = filter isProducer types
		removeInputs = filter isOutput types
		in (finaltype, joins finaltype removeConsumer : flows finaltype removeInputs)
	where
		flows ft types = map (insertFlows ft) types
		joins ft types = JoinRelation ft types

-- insert flows from and to final type
insertFlows :: FinalType -> Type -> TypingRelation
insertFlows finalType (VarType name ident mode position)
	-- Producers flow to final type
	| position == Producer = FlowRelation (VarType name ident mode position) finalType
	-- final types flow to consumers
	-- position == Consumer
	| otherwise = FlowRelation finalType (VarType name ident mode position)
-- never happens
insertFlows finalType typ = FlowRelation finalType typ

-- Step 4.4 - Replace input variables with final Type

-- Replace type variable ocurrences in input position with final type
replaceInputVariable :: TypingRelation -> String -> Type -> TypingRelation
replaceInputVariable (TypeAssignment ctx expr typ) targetVar finalType =
	let
		ctx' = map (\x -> step4_4Context x targetVar finalType) ctx
		expr' = step4_4Expression expr targetVar finalType
		typ' = step4_4Type typ targetVar finalType
	in (TypeAssignment ctx' expr' typ')
replaceInputVariable (MatchingRelation var typ) targetVar finalType =
	let
		typ' = step4_4Type typ targetVar finalType
	in (MatchingRelation var typ')
replaceInputVariable typingrelation _ _ = typingrelation

-- apply step 4.4 to context
step4_4Context :: Bindings -> String -> Type -> Bindings
step4_4Context (Context ctx) _ _ = Context ctx
step4_4Context (Binding var typ) targetVar finalType =
	Binding var $ step4_4Type typ targetVar finalType

-- apply step 4.4 to expression
step4_4Expression :: Expression -> String -> Type -> Expression
step4_4Expression (Var name cast) _ _ = Var name cast
step4_4Expression (Abstraction var expr) targetVar finalType =
	(Abstraction var expr')
	where expr' = step4_4Expression expr targetVar finalType
step4_4Expression (Application expr1 expr2) targetVar finalType =
	let
		expr1' = step4_4Expression expr1 targetVar finalType
		expr2' = step4_4Expression expr2 targetVar finalType
	in Application expr1' expr2'
step4_4Expression (Function name typeAnnotation exprs) targetVar finalType
	| isJust typeAnnotation =
		Function name (Just $ step4_4Type (fromJust typeAnnotation) targetVar finalType) expr'
	| otherwise = func
	where
		expr' = map (\x -> step4_4Expression x targetVar finalType) exprs
		func = Function name typeAnnotation expr'

-- apply step 4.4 to type
-- most rules only propagate the changes to the types
step4_4Type :: Type -> String -> Type -> Type
step4_4Type (BaseType name mode position) targetVar finalType =
	BaseType name mode position
-- apply name and identifier changes
step4_4Type (VarType name ident mode position) targetVar finalType
	-- if type variable is in input mode and the name is the same
	| mode == Input && name == targetVar =
		-- change name and identifier, remove mode and position
		--(VarType finalTypeName finalTypeIdent mode position)
		(VarType finalTypeName finalTypeIdent NullMode NullPosition)
	| otherwise = (VarType name ident mode position)
	where
		finalTypeName = typeName finalType
		finalTypeIdent = typeIdent finalType
step4_4Type (DynType) targetVar finalType = DynType
step4_4Type (ArrowType type1 type2) targetVar finalType =
	let
		type1' = step4_4Type type1 targetVar finalType
		type2' = step4_4Type type2 targetVar finalType
	in (ArrowType type1' type2')
step4_4Type (ListType typ) targetVar finalType =
	let
		typ' = step4_4Type typ targetVar finalType
	in ListType typ'
step4_4Type (PairType type1 type2) targetVar finalType =
	let
		type1' = step4_4Type type1 targetVar finalType
		type2' = step4_4Type type2 targetVar finalType
	in (PairType type1' type2')
step4_4Type (RefType typ) targetVar finalType =
	let
		typ' = step4_4Type typ targetVar finalType
	in RefType typ'
step4_4Type (SumType type1 type2) targetVar finalType =
	let
		type1' = step4_4Type type1 targetVar finalType
		type2' = step4_4Type type2 targetVar finalType
	in (SumType type1' type2')
step4_4Type (TypeConstructor name types) targetVar finalType =
	let
		types' = map (\x -> step4_4Type x targetVar finalType) types
	in TypeConstructor name types'
