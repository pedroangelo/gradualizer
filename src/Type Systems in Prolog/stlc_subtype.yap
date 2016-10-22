%% Simply Typed Lambda Calculus type system
%% Subtyping

type(Context, var(X),	T) :- lookup(X:T, Context).
type(Context, abs(X, E), arrow(T1, T2)) :- type([X:T1 | Context], E, T2).
type(Context, app(E1, E2), B) :- type(Context, E1, arrow(A, B)),
								 type(Context, E2, T),
								 subtype(A, T).

lookup(X:T, [Var:Type | Context]) :- X = Var, T = Type.
lookup(X:T, [Var:Type | Context]) :- X \== Var, lookup(X:T, Context).
