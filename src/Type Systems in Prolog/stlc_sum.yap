%% Simply Typed Lambda Calculus type system
%% Sum type

type(Context, inl(E), sumType(T1, T2)) :- type(Context, E, T1).
type(Context, inr(E), sumType(T1, T2)) :- type(Context, E, T2).
type(Context, case(E, E1, E2), T) :- type(Context, E, sumType(T1, T2)),
									 type([X1:T1 | Context], E1, T),
									 type([X2:T2 | Context], E2, T).
