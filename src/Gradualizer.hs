module Gradualizer (
	generateGradual,
	generateCompiler,
	gradualize,
	compilerToCC
) where

-- Type Systems
import InitialTypeSystem as ITS
import GradualTypeSystem as GTS
import CastCalculus as CC

-- Parsers
import PrologParser
import SignatureParser

-- Gradulizer Steps
import Classify
import PatternMatch
import Flows
import LoneInputs
import Consistency
import PatternMatchRules
import CastInsertion

-- Imports
import Data.Maybe

-- read type system from prolog file,
-- parse contents and convert to TypeSystem,
-- then gradualize
generateGradual :: FilePath -> IO GTS.TypeSystem
generateGradual file = do
	-- parse prolog to Program format
	prolog <- parseProlog file
	-- parse signatures to Signatures format
	signatures <- parseSignatures file
	-- get type annotated terms
	let typeAnnotatedTerms = deriveTypeAnnotations signatures
	-- convert Program format to Initial Type System using annotated terms information
	let ts = ITS.toTypeSystem prolog typeAnnotatedTerms
	-- gradualize typesystem
	return $ gradualize signatures ts

-- gradualize type system by applying 6 steps:
-- - Step 1: Classify type variables with input or output modes
-- - Step 2: Classify type variables with producer or consumer positions
-- - Step 3: Apply pattern matching to constructed outputs
-- - Step 4: Flow and final type discovery
-- - Step 5: Restrict lone inputs to be static
-- - Step 6: Replace flow with consistency
-- - Step PM: Add pattern matching rules
-- - Step Final: Remove mode and position from type variables and flow relation from typing relation
gradualize :: Signatures -> ITS.TypeSystem -> GTS.TypeSystem
gradualize signatures =
	-- step Final
	GTS.convertTypeSystem .
	-- step PM
	addPatternMatchRules signatures .
	-- step 6
	removeFlowsInsertConsistency .
	-- step 5
	loneInputsStatic .
	-- step 4
	insertFlowsFinalType .
	-- step 3
	applyPatternMatching .
	-- step 1 and 2
	classifyTypeVariables

-- read type system from prolog file,
-- parse contents and convert to TypeSystem,
-- then generate compiler to cast calculus
generateCompiler :: FilePath -> IO CC.TypeSystem
generateCompiler file = do
	-- parse prolog to Program format
	prolog <- parseProlog file
	-- parse signatures to Signatures format
	signatures <- parseSignatures file
	-- get type annotated terms
	let typeAnnotatedTerms = deriveTypeAnnotations signatures
	-- convert Program format to Initial Type System using annotated terms information
	let ts = ITS.toTypeSystem prolog typeAnnotatedTerms
	-- generate compiler to cast calculus
	return $ compilerToCC signatures ts

-- generate a compiler to cast calculus by applying 6 steps:
-- - Step 1: Classify type variables with input or output modes
-- - Step 2: Classify type variables with producer or consumer positions
-- - Step 3: Apply pattern matching to constructed outputs
-- - Step 4: Flow and final type discovery
-- - Step 5: Restrict lone inputs to be static
-- - Step 7: Generate casts as directed by the flow premises
-- - Step 6: Replace flow with consistency
-- - Step Final: Remove mode and position from type variables and flow relation from typing relation
compilerToCC :: Signatures -> ITS.TypeSystem -> CC.TypeSystem
compilerToCC signatures =
	-- step Final
	CC.convertTypeSystem .
	-- step 6
	removeFlowsInsertConsistency .
	-- step 7
	generateCasts .
	-- step 5
	loneInputsStatic .
	-- step 4
	insertFlowsFinalType .
	-- step 3
	applyPatternMatching .
	-- step 1 and 2
	classifyTypeVariables
