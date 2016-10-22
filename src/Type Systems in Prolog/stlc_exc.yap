%% Extension to Simply Typed Lambda Calculus type system
%% Exceptions

type(Context, raise(T, E), T) :- type(Context, E, excType).
type(Context, try(E1, E2), T) :- type(Context, E1, T),
								 type(Context, E2, arrow(excType, T)).
