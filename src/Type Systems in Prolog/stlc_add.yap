%% Extension to Simply Typed Lambda Calculus type system
%% Addition and Integers

type(Context, zero, int).
type(Context, succ(E), int) :- type(Context, E, int).
type(Context, add(E1, E2), int) :- type(Context, E1, int),
								   type(Context, E2, int).
