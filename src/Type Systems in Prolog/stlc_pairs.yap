%% Extension to Simply Typed Lambda Calculus type system
%% Pairs

type(Context, fst(E), T1) :- type(Context, E, pairType(T1, T2)).
type(Context, snd(E), T2) :- type(Context, E, pairType(T1, T2)).
type(Context, pair(E1, E2), pairType(T1, T2)) :- type(Context, E1, T1),
												 type(Context, E2, T2).
