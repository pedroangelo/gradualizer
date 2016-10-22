module StaticTypeSystemExamples where

import InitialTypeSystem

-- simply typed lambda calculus
stlc :: TypeSystem
stlc = TypeSystem
	[TypeRule
		[]
		(TypeAssignment
			([Context "Γ", Binding "x" (VarType "T" "" NullMode NullPosition)])
			(Var "x")
			(VarType "T" "" NullMode NullPosition)),
	TypeRule
		[TypeAssignment
			([Context "Γ", Binding "x" (VarType "T1" "" NullMode NullPosition)])
			(Var "e")
			(VarType "T2" "" NullMode NullPosition)
		]
		(TypeAssignment
			([Context "Γ"])
			(Abstraction ("x") (Var "e"))
			(ArrowType (VarType "T1" "" NullMode NullPosition) (VarType "T2" "" NullMode NullPosition))),
	TypeRule
		[TypeAssignment
			([Context "Γ"])
			(Var "e1")
			(ArrowType (VarType "T1" "" NullMode NullPosition) (VarType "T2" "" NullMode NullPosition)),
		TypeAssignment
			([Context "Γ"])
			(Var "e2")
			(VarType "T1" "" NullMode NullPosition)
		]
		(TypeAssignment
			([Context "Γ"])
			(Application (Var "e1") (Var "e2"))
			(VarType "T2" "" NullMode NullPosition))
	]

stlc_add :: TypeSystem
stlc_add = TypeSystem
	[TypeRule
		[]
		(TypeAssignment
			([Context "Γ"])
			(Function "zero" Nothing [])
			(BaseType "int" NullMode NullPosition)),
	TypeRule
		[TypeAssignment
			([Context "Γ"])
			(Var "e")
			(BaseType "int" NullMode NullPosition)
		]
		(TypeAssignment
			([Context "Γ"])
			(Function "succ" Nothing [Var "e"])
			(BaseType "int" NullMode NullPosition)),
	TypeRule
		[TypeAssignment
			([Context "Γ"])
			(Var "e1")
			(BaseType "int" NullMode NullPosition),
		TypeAssignment
			([Context "Γ"])
			(Var "e2")
			(BaseType "int" NullMode NullPosition)
		]
		(TypeAssignment
			([Context "Γ"])
			(Function "add" Nothing [Var "e1", Var "e2"])
			(BaseType "int" NullMode NullPosition))
	]

stlc_bool :: TypeSystem
stlc_bool = TypeSystem
	[TypeRule
		[]
		(TypeAssignment
			([Context "Γ"])
			(Function "true" Nothing [])
			(BaseType "bool" NullMode NullPosition)),
	TypeRule
		[]
		(TypeAssignment
			([Context "Γ"])
			(Function "false" Nothing [])
			(BaseType "bool" NullMode NullPosition))
	]

stlc_exc :: TypeSystem
stlc_exc = TypeSystem
	[TypeRule
		[TypeAssignment
			([Context "Γ"])
			(Var "e")
			(BaseType "excType" NullMode NullPosition)
		]
		(TypeAssignment
			([Context "Γ"])
			(Function "raise" (Just $ VarType "T" "" NullMode NullPosition) [Var "e"])
			(VarType "T" "" NullMode NullPosition)),
	TypeRule
		[TypeAssignment
			([Context "Γ"])
			(Var "e1")
			(VarType "T" "" NullMode NullPosition),
		TypeAssignment
			([Context "Γ"])
			(Var "e2")
			(ArrowType (BaseType "excType" NullMode NullPosition) (VarType "T" "" NullMode NullPosition))
		]
		(TypeAssignment
			([Context "Γ"])
			(Function "try" Nothing [Var "e1", Var "e2"])
			(VarType "T" "" NullMode NullPosition))
	]

stlc_fix :: TypeSystem
stlc_fix = TypeSystem
	[TypeRule
		[TypeAssignment
			([Context "Γ"])
			(Var "e")
			(ArrowType (VarType "T" "" NullMode NullPosition) (VarType "T" "" NullMode NullPosition))
		]
		(TypeAssignment
			([Context "Γ"])
			(Function "fix" Nothing [Var "e"])
			(VarType "T" "" NullMode NullPosition))
	]

stlc_if :: TypeSystem
stlc_if = TypeSystem
	[TypeRule
		[TypeAssignment
			([Context "Γ"])
			(Var "e1")
			(BaseType "bool" NullMode NullPosition),
		TypeAssignment
			([Context "Γ"])
			(Var "e2")
			(VarType "T" "" NullMode NullPosition),
		TypeAssignment
			([Context "Γ"])
			(Var "e3")
			(VarType "T" "" NullMode NullPosition)
		]
		(TypeAssignment
			([Context "Γ"])
			(Function "if" Nothing [Var "e1", Var "e2", Var "e3"])
			(VarType "T" "" NullMode NullPosition))
	]

stlc_let :: TypeSystem
stlc_let = TypeSystem
	[TypeRule
		[TypeAssignment
			([Context "Γ"])
			(Var "e1")
			(VarType "T1" "" NullMode NullPosition),
		TypeAssignment
			([Context "Γ", Binding "x" (VarType "T1" "" NullMode NullPosition)])
			(Var "e2")
			(VarType "T2" "" NullMode NullPosition)
		]
		(TypeAssignment
			([Context "Γ"])
			(Function "let" Nothing [Var "x", Var "e1", Var "e2"])
			(VarType "T2" "" NullMode NullPosition))
	]

stlc_lists :: TypeSystem
stlc_lists = TypeSystem
	[TypeRule
		[]
		(TypeAssignment
			([Context "Γ"])
			(Function "emptyList" (Just $ VarType "T" "" NullMode NullPosition) [])
			(ListType (VarType "T" "" NullMode NullPosition))),
	TypeRule
		[TypeAssignment
			([Context "Γ"])
			(Var "e")
			(ListType (VarType "T" "" NullMode NullPosition))
		]
		(TypeAssignment
			([Context "Γ"])
			(Function "isnil" (Just $ VarType "T" "" NullMode NullPosition) [Var "e"])
			(BaseType "bool" NullMode NullPosition)),
	TypeRule
		[TypeAssignment
			([Context "Γ"])
			(Var "e1")
			(VarType "T" "" NullMode NullPosition),
		TypeAssignment
			([Context "Γ"])
			(Var "e2")
			(ListType (VarType "T" "" NullMode NullPosition))
		]
		(TypeAssignment
			([Context "Γ"])
			(Function "cons" (Just $ VarType "T" "" NullMode NullPosition) [Var "e1", Var "e2"])
			(ListType (VarType "T" "" NullMode NullPosition))),
	TypeRule
		[TypeAssignment
			([Context "Γ"])
			(Var "e")
			(ListType (VarType "T" "" NullMode NullPosition))
		]
		(TypeAssignment
			([Context "Γ"])
			(Function "head" (Just $ VarType "T" "" NullMode NullPosition) [Var "e"])
			(VarType "T" "" NullMode NullPosition)),
	TypeRule
		[TypeAssignment
			([Context "Γ"])
			(Var "e")
			(ListType (VarType "T" "" NullMode NullPosition))
		]
		(TypeAssignment
			([Context "Γ"])
			(Function "tail" (Just $ VarType "T" "" NullMode NullPosition) [Var "e"])
			(ListType (VarType "T" "" NullMode NullPosition)))
	]

stlc_pairs :: TypeSystem
stlc_pairs = TypeSystem
	[TypeRule
		[TypeAssignment
			([Context "Γ"])
			(Var "e")
			(PairType (VarType "T1" "" NullMode NullPosition) (VarType "T2" "" NullMode NullPosition))
		]
		(TypeAssignment
			([Context "Γ"])
			(Function "fst" Nothing [Var "e"])
			(VarType "T1" "" NullMode NullPosition)),
	TypeRule
		[TypeAssignment
			([Context "Γ"])
			(Var "e")
			(PairType (VarType "T1" "" NullMode NullPosition) (VarType "T2" "" NullMode NullPosition))
		]
		(TypeAssignment
			([Context "Γ"])
			(Function "snd" Nothing [Var "e"])
			(VarType "T2" "" NullMode NullPosition)),
	TypeRule
		[TypeAssignment
			([Context "Γ"])
			(Var "e1")
			(VarType "T1" "" NullMode NullPosition),
		TypeAssignment
			([Context "Γ"])
			(Var "e2")
			(VarType "T2" "" NullMode NullPosition)
		]
		(TypeAssignment
			([Context "Γ"])
			(Function "pair" Nothing [Var "e1", Var "e2"])
			(PairType (VarType "T1" "" NullMode NullPosition) (VarType "T2" "" NullMode NullPosition)))
	]

stlc_ref :: TypeSystem
stlc_ref = TypeSystem
	[TypeRule
		[TypeAssignment
			([Context "Γ"])
			(Var "e")
			(VarType "T" "" NullMode NullPosition)
		]
		(TypeAssignment
			([Context "Γ"])
			(Function "ref" Nothing [Var "e"])
			(RefType (VarType "T" "" NullMode NullPosition))),
	TypeRule
		[TypeAssignment
			([Context "Γ"])
			(Var "e")
			(RefType (VarType "T" "" NullMode NullPosition))
		]
		(TypeAssignment
			([Context "Γ"])
			(Function "deref" Nothing [Var "e"])
			(VarType "T" "" NullMode NullPosition)),
	TypeRule
		[TypeAssignment
			([Context "Γ"])
			(Var "e1")
			(RefType (VarType "T" "" NullMode NullPosition)),
		TypeAssignment
			([Context "Γ"])
			(Var "e2")
			(VarType "T" "" NullMode NullPosition)
		]
		(TypeAssignment
			([Context "Γ"])
			(Function "assign" Nothing [Var "e1", Var "e2"])
			(BaseType "unitType" NullMode NullPosition))
	]
{-
stlc_subtype :: TypeSystem
stlc_subtype = TypeSystem
	[TypeRule
		[
		[Context "Γ", Binding "x" (VarType "T1" "")] Var "e" VarType "T2" ""
		]
		[Context "Γ"] Abstraction ("x", VarType "T1" "") (Var "e") ArrowType [VarType "T1" "", VarType "T2" ""],
	TypeRule
		[
		[Context "Γ"] Var "e1" ArrowType [VarType "A", VarType "B"],
		[Context "Γ"] Var "e2" VarType "T" "",
		[] Function "subtype" Nothing [Var "A", Var "T"] VarType ""
		]
		[Context "Γ"] Application (Var "e1") (Var "e2") VarType "B"
	]
-}
stlc_unit :: TypeSystem
stlc_unit = TypeSystem
	[TypeRule
		[]
		(TypeAssignment
			([Context "Γ"])
			(Function "unit" Nothing [])
			(BaseType "unitType" NullMode NullPosition))
	]
