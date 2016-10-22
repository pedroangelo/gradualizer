%% Extension to Simply Typed Lambda Calculus type system
%% Conditional Statements

:- [stlc_bool].

type(Context, if(E1, E2, E3), T) :- type(Context, E1, bool),
									type(Context, E2, T),
									type(Context, E3, T).
