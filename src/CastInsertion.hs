module CastInsertion where

-- Type System
import InitialTypeSystem

-- Imports
import Control.Monad.State
import Data.Maybe

-- Step 7: Generate casts as directed by the flow premises

-- TODO:
-- - Change cast insertion according to variance of the type (step 7.2)
-- - Derive casts for pattern match variables

-- generate casts according to flow premises and pattern match relations
generateCasts :: TypeSystem -> TypeSystem
generateCasts (TypeSystem typerules) = TypeSystem $ map step7TypeRule typerules

-- apply step 7 to type rules
step7TypeRule :: TypeRule -> TypeRule
step7TypeRule (TypeRule premises conclusion) = do
	-- collect all types associated with expression variables (step 7.1)
	let expressionCollection = snd $ runState (collectExpressions (TypeRule premises conclusion)) []
	-- find cast destination for each expression and return all expressions with casts (step 7.2)
	let expressions = map (\x -> findCasts x premises) expressionCollection
	-- replace expressions with compilation (step 7.3)
	TypeRule (map step7_3Premise premises) (step7_3Conclusion expressions conclusion)

type ExpressionCollection = [ExpressionType]
type ExpressionType = (Expression, Type)

-- Step 7.1 - Collect all expressions and associated types from premise typing relations

-- apply step 7.1 to type rules
collectExpressions :: TypeRule -> State ExpressionCollection ()
collectExpressions (TypeRule premises conclusion) = do
	mapM_ step7_1Relation premises

-- apply step 7.1 to a typing relation
step7_1Relation :: TypingRelation -> State ExpressionCollection ()
-- if typing relation is type assignment
-- and the type assignment's expression is a type variable
step7_1Relation (TypeAssignment ctx (Var name _) typ) = do
	expressionCollection <- get
	-- collect the expression
	put (expressionCollection ++ [(Var name Nothing, typ)])
-- otherwise do nothing
step7_1Relation _ = return ()

-- Step 7.2 - Find cast destinations for given expression

-- given a expression with a type, find the final cast destination for such expression
findCasts :: ExpressionType -> [TypingRelation] -> Expression
findCasts (Var name _, typ) trs
	-- if the found type (final type) is equal to the initial type, then no need to add a cast
	| isEqualType typ finalType = Var name Nothing
	-- otherwise, add a cast from initial type to final type and return expression
	| otherwise = (Var name $ Just (typ, finalType))
	-- find final cast destination for the given type
	where finalType = castDestination typ trs


-- get the final cast destination for a given type
castDestination :: Type -> [TypingRelation] -> Type
castDestination typ trs
	-- if next type is equal to typ, then nextType is the final type
	| isEqualType typ nextType = nextType
	-- otherwise recursivelly search for next type
	| otherwise = castDestination nextType trs
	-- get next type following casts
	where nextType = findCast typ trs

-- Find next cast destination
-- navigate accross constructed types until reaching type variable
findCast :: Type -> [TypingRelation] -> Type
findCast (BaseType name mode position) _ = BaseType name mode position
findCast (VarType name ident mode position) trs = findCast' (VarType name ident mode position) trs
findCast (DynType) _ = DynType
findCast (ArrowType type1 type2) trs = ArrowType (findCast type1 trs) (findCast type2 trs)
findCast (ListType type1) trs = ListType (findCast type1 trs)
findCast (PairType type1 type2) trs = PairType (findCast type1 trs) (findCast type2 trs)
findCast (RefType type1) trs = RefType (findCast type1 trs)
findCast (SumType type1 type2) trs = SumType (findCast type1 trs) (findCast type2 trs)
findCast (TypeConstructor name types) trs = TypeConstructor name (map (\x -> findCast x trs) types)

-- Find next cast destination by either following flows or pattern match relations
findCast' :: Type -> [TypingRelation] -> Type
findCast' typ [] = typ
-- if we came accross a flow relation
findCast' typ ((FlowRelation type1 type2) : premises)
	-- and the initial type is equal to the first type in the flow, return second type
	| isEqualType typ type1 = type2
	-- otherwise continue the search
	| otherwise = findCast' typ premises
-- if we came accross a matching relation
findCast' typ ((MatchingRelation type1 type2) : premises)
	-- and the initial type is equal to the left type in the matchinf relation, return right type
	| isEqualType typ type1 = type2
	-- otherwise continue the search
	| otherwise = findCast' typ premises
-- if we came accross other typing relation, just ignore and continue the search
findCast' typ (_ : premises) = findCast' typ premises

-- Step 7.3 - Replace expressions with compilation from expressions to expressions with casts
-- both in premises and in the conclusion

-- replace expressions with compilation
-- from expressions to expressions in premises (expr ⇝ expr')
step7_3Premise :: TypingRelation -> TypingRelation
step7_3Premise (TypeAssignment ctx expr typ) =
	TypeAssignment ctx (step7_3PExpression expr) typ
step7_3Premise tr = tr

-- apply step 7.3 to expressions in premises
step7_3PExpression :: Expression -> Expression
step7_3PExpression (Var name cast) =
	Compilation (Var name cast) (Var (name ++ "'") cast)
step7_3PExpression (Abstraction var expr) =
	Abstraction var (step7_3PExpression expr)
step7_3PExpression (Application expr1 expr2) =
	Application (step7_3PExpression expr1) (step7_3PExpression expr1)
step7_3PExpression (Function name typeAnnotation exprs) =
	Function name typeAnnotation (map step7_3PExpression exprs)
step7_3PExpression expr = expr

-- replace expressions with compilation
-- from expressions to expressions with casts in conclusion (expr ⇝ (expr' : T => T))
step7_3Conclusion :: [Expression] -> Conclusion -> Conclusion
step7_3Conclusion castExprs (TypeAssignment ctx expr typ) =
	TypeAssignment ctx (Compilation expr (step7_3CExpression castExprs expr)) typ
step7_3Conclusion _ tr = tr

-- apply step 7.3 to expressions in conclusion
step7_3CExpression :: [Expression] -> Expression -> Expression
step7_3CExpression castExprs (Var name cast) =
	getExpression (Var name cast) castExprs
step7_3CExpression castExprs (Abstraction name expr) =
	Abstraction name (step7_3CExpression castExprs expr)
step7_3CExpression castExprs (Application expr1 expr2) =
	Application (step7_3CExpression castExprs expr1) (step7_3CExpression castExprs expr2)
step7_3CExpression castExprs (Function name typeAnnotation exprs) =
	Function name typeAnnotation (map (step7_3CExpression castExprs) exprs)
step7_3CExpression _ expr = expr

-- given a expression, look for the same expression in the list of expressions with casts
getExpression :: Expression -> [Expression] -> Expression
-- if no expression is found, then there is no compilation, just return same expression
getExpression (Var name cast) [] = (Var name cast)
-- iterate and compare expressions in a recursive manner
getExpression (Var name cast) ((Var name' cast') : exprs)
	-- if the names of the expressions are the same,
	-- just return the casted expression, adding a ' to the name
	| name == name' = (Var (name' ++ "'") cast')
	-- otherwise continue the search
	| otherwise = getExpression (Var name cast) exprs
