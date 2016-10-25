module PatternMatchRules (
	addPatternMatchRules
) where

-- Type System
import InitialTypeSystem

-- Parsers
import SignatureParser

-- Step PM: Add pattern matching rules

-- add pattern matching rules to type system according to signatures
addPatternMatchRules :: Signatures -> TypeSystem -> TypeSystem
addPatternMatchRules (Signatures signatures) (TypeSystem typeRules) =
	-- first derive pattern matching rules for each type
	let patternMatchRules = concat $ map addPatternMatchRelation signatures
	-- add to type system
	in TypeSystem (typeRules ++ patternMatchRules)

-- Derive pattern matching rules for a given signatures
addPatternMatchRelation :: Signature -> [TypeRule]
addPatternMatchRelation (Signature name kind args)
	-- if signature corresponds to a Type
	| kind == "Type" =
		let
			-- build type with new variables
			typ = buildType name (length args)
			-- build type with dynamic type
			typDyn = buildTypeDyn name (length args)
			-- build pattern match rule for dynamic type
			dynRelation = TypeRule [] (MatchingRelation DynType typDyn)
			-- build pattern match rule for variable types
			typRelation = TypeRule [] (MatchingRelation typ typ)
		in [typRelation, dynRelation]
	| otherwise = []

-- build type, if is constructed instantiate with type variables
buildType :: String -> Int -> Type
buildType name 0 = BaseType name NullMode NullPosition
buildType "arrow" 2 = ArrowType (newVar "T1" "") (newVar "T2" "")
buildType "list" 1 = ListType (newVar "T" "")
buildType "pairType" 2 = PairType (newVar "T1" "") (newVar "T2" "")
buildType "refType" 1 = RefType (newVar "T" "")
buildType "sumType" 2 = SumType (newVar "T1" "") (newVar "T2" "")
buildType name numArgs = TypeConstructor name args
	where args = map (\x -> newVar ("T" ++ x) "") (map show [1..numArgs])

-- build type, if is constructed instantiate with dynamic type
buildTypeDyn :: String -> Int -> Type
buildTypeDyn name 0 = BaseType name NullMode NullPosition
buildTypeDyn "arrow" 2 = ArrowType DynType DynType
buildTypeDyn "list" 1 = ListType DynType
buildTypeDyn "pairType" 2 = PairType DynType DynType
buildTypeDyn "refType" 1 = RefType DynType
buildTypeDyn "sumType" 2 = SumType DynType DynType
buildTypeDyn name numArgs = TypeConstructor name args
	where args = replicate numArgs (DynType)
