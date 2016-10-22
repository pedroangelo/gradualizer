%% Extension to Simply Typed Lambda Calculus type system
%% Let Rec

:- [stlc, stlc_let, stlc_fix].

type(Context, letrec(X, E1, E2), T2) :- type([X:T1 | Context], E1, T1),
										type([X:T1 | Context], E2, T2).
