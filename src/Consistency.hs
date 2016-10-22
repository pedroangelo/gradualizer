module Consistency (
	removeFlowsInsertConsistency
) where

-- Type System
import InitialTypeSystem

-- Step 6: Remove flows to join types and replace remaining flows with consistency checks

-- TODO:

-- Apply step 6 to a type system
removeFlowsInsertConsistency :: TypeSystem -> TypeSystem
removeFlowsInsertConsistency (TypeSystem typerules) = TypeSystem $ map step6TypeRule typerules

-- Apply step 6 to a type rule
step6TypeRule :: TypeRule -> TypeRule
step6TypeRule (TypeRule premises conclusion) = TypeRule (step6Premises premises) conclusion

-- Apply step 6 to premises
step6Premises :: Premises -> Premises
step6Premises [] = []
-- If typing relation is a flow
step6Premises ((FlowRelation type1 type2) : premises)
	-- remove flows to join types
	| isJoinType type2 = step6Premises premises
	-- replace remaining flows with consistency checks
	| otherwise = (ConsistencyRelation type1 type2) : step6Premises premises
-- do nothing if typing relation is not flow
step6Premises (premise : premises) = premise : step6Premises premises
