%% Extension to Simply Typed Lambda Calculus type system
%% Let Bindings

type(Context, let(X, E1, E2), T2) :- type(Context, E1, T1),
									 type([X:T1 | Context], E2, T2).
