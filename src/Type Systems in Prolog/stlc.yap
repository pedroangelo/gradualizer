%% Simply Typed Lambda Calculus type system
%type([X:T | Context], var(X), T).
%type(Context, var(X),	T) :- member(X:T, Context).
%type(Context, abs(X, E), arrow(T1, T2)) :- type([X:T1 | Context], E, T2).
%type(Context, app(E1, E2), B) :- type(Context, E1, arrow(A, B)),
%								 type(Context, E2, A).
%
%member(X:T, [Var:Type | Context]) :- X = Var, T = Type.
%member(X:T, [Var:Type | Context]) :- X \== Var, member(X:T, Context).

type(Context, var(X),	T) :- member(X:T, Context).
type(Context, abs(X, E), arrow(T1, T2)) :- type([X:T1 | Context], E, T2).
type(Context, app(E1, E2), B) :- type(Context, E1, arrow(A, B)),
								 type(Context, E2, A).
