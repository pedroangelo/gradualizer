module PatternMatch (
	applyPatternMatching
) where

-- Type System
import InitialTypeSystem

-- Imports
import Control.Monad.State

-- Step 3: Replace constructed outputs with fresh type variables
-- and pattern match these variables with the constructed outputs

-- TODO:
-- - check if pattern match is only needed in premises
-- - consider the case that typing relations can be others
-- aside from type assignment or matching relation (step3Premises)

-- apply pattern match to type system
applyPatternMatching :: TypeSystem -> TypeSystem
applyPatternMatching (TypeSystem typerules) = TypeSystem $ map step3TypeRules typerules

-- apply pattern match to type rule
-- start State monad, we only need diferent type variables inside each type rule
step3TypeRules :: TypeRule -> TypeRule
step3TypeRules (TypeRule premises conclusion) =
	-- start State Monad to name new pattern match variables
	TypeRule (fst $ runState (step3Premises premises) 0) conclusion

-- apply pattern match to premises
step3Premises :: Premises -> State Int Premises
step3Premises [] = return []
-- if typing relation is a type assignment
step3Premises ((TypeAssignment ctx expr typ) : premises) = do
	-- pattern match over type variables, replacing constructed outputs with fresh type variables
	(pmtype, pmpremises) <- patternMatchTypeAssignment typ
	-- pattern match over newly created typing relations, such as MatchingRelation
	pmpremises' <- step3Premises pmpremises
	-- pattern match over the rest of the premises
	premises' <- step3Premises premises
	return $ [(TypeAssignment ctx expr pmtype)] ++ pmpremises' ++ premises'
-- if typing relation if a matching relation
step3Premises ((MatchingRelation var typ) : premises) = do
	-- pattern match over type variables inside constructed outputs,
	-- replacing such variables with fresh type variables
	(pmtype, pmpremises) <- patternMatchConstructedOutput typ
	-- pattern match over the rest of the premises
	premises' <- step3Premises premises
	return $ [(MatchingRelation var pmtype)] ++ pmpremises ++ premises
-- do not classify variables in member relation
step3Premises ((MemberRelation elem1 elem2) : premises) = do
	premises' <- step3Premises premises
	return $ (MemberRelation elem1 elem2) : premises'
-- if not typing relation or matching relation
step3Premises _ = return []

-- pattern match over constructed types such as arrow or base types
-- check if variables inside constructed types are outputs
-- create fresh pattern match variables, assigning diferent numbers
patternMatchTypeAssignment :: Type -> State Int (Type, Premises)
patternMatchTypeAssignment (BaseType typ mode position)
	| mode == Output = do
		i <- get
		put (i+1)
		return (pm i, [MatchingRelation (pm i) (BaseType typ mode position)])
	| otherwise = return ((BaseType typ mode position), [])
patternMatchTypeAssignment (VarType typ ident mode position) =
	return ((VarType typ ident mode position), [])
patternMatchTypeAssignment (DynType) = return ((DynType), [])
patternMatchTypeAssignment (ArrowType type1 type2)
	| isOutputType type1 && isOutputType type2 = do
		i <- get
		put (i+1)
		return (pm i, [MatchingRelation (pm i) (ArrowType type1 type2)])
	| otherwise = return ((ArrowType type1 type2), [])
patternMatchTypeAssignment (ListType type_)
	| isOutputType type_ = do
		i <- get
		put (i+1)
		return (pm i, [MatchingRelation (pm i) (ListType type_)])
	| otherwise = return ((ListType type_), [])
patternMatchTypeAssignment (PairType type1 type2)
	| isOutputType type1 && isOutputType type2 = do
		i <- get
		put (i+1)
		return (pm i, [MatchingRelation (pm i) (PairType type1 type2)])
	| otherwise = return ((PairType type1 type2), [])
patternMatchTypeAssignment (RefType type_)
	| isOutputType type_ = do
		i <- get
		put (i+1)
		return (pm i, [MatchingRelation (pm i) (RefType type_)])
	| otherwise = return ((RefType type_), [])
patternMatchTypeAssignment (SumType type1 type2)
	| isOutputType type1 && isOutputType type2 = do
		i <- get
		put (i+1)
		return (pm i, [MatchingRelation (pm i) (SumType type1 type2)])
	| otherwise = return ((SumType type1 type2), [])
patternMatchTypeAssignment (TypeConstructor name types)
	| all isOutputType types = do
		i <- get
		put (i+1)
		return (pm i, [MatchingRelation (pm i) (TypeConstructor name types)])
	| otherwise = return ((TypeConstructor name types), [])

-- pattern match over constructed outputs already pattern matched
patternMatchConstructedOutput :: Type -> State Int (Type, Premises)
patternMatchConstructedOutput (BaseType type_ mode position) =
	return ((BaseType type_ mode position), [])
patternMatchConstructedOutput (VarType typ ident mode position) =
	return ((VarType typ ident mode position), [])
patternMatchConstructedOutput (DynType) = return ((DynType), [])
patternMatchConstructedOutput (ArrowType type1 type2)
	| isOutputType type1 && isOutputType type2 = do
		(pmtype1, pmpremises1) <- patternMatchTypeAssignment type1
		(pmtype2, pmpremises2) <- patternMatchTypeAssignment type2
		return ((ArrowType pmtype1 pmtype2), pmpremises1 ++ pmpremises2)
	| otherwise = return ((ArrowType type1 type2), [])
patternMatchConstructedOutput (ListType type_)
	| isOutputType type_ = do
		(pmtype, pmpremises) <- patternMatchTypeAssignment type_
		return ((ListType pmtype), pmpremises)
	| otherwise = return ((ListType type_), [])
patternMatchConstructedOutput (PairType type1 type2)
	| isOutputType type1 && isOutputType type2 = do
		(pmtype1, pmpremises1) <- patternMatchTypeAssignment type1
		(pmtype2, pmpremises2) <- patternMatchTypeAssignment type2
		return ((PairType pmtype1 pmtype2), pmpremises1 ++ pmpremises2)
	| otherwise = return ((PairType type1 type2), [])
patternMatchConstructedOutput (RefType type_)
	| isOutputType type_ = do
		(pmtype, pmpremises) <- patternMatchTypeAssignment type_
		return ((RefType pmtype), pmpremises)
	| otherwise = return ((RefType type_), [])
patternMatchConstructedOutput (SumType type1 type2)
	| isOutputType type1 && isOutputType type2 = do
		(pmtype1, pmpremises1) <- patternMatchTypeAssignment type1
		(pmtype2, pmpremises2) <- patternMatchTypeAssignment type2
		return ((SumType pmtype1 pmtype2), pmpremises1 ++ pmpremises2)
	| otherwise = return ((SumType type1 type2), [])
patternMatchConstructedOutput (TypeConstructor name types)
	| all isOutputType types = do
		list <- mapM patternMatchTypeAssignment types
		return ((TypeConstructor name (map fst list)), concat $ map snd list)
	| otherwise = return ((TypeConstructor name types), [])
