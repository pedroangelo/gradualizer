%% Extension to Simply Typed Lambda Calculus type system
%% Lists

:- [stlc_bool].

type(Context, emptyList(T), list(T)).
type(Context, isnil(T, E), bool) :- type(Context, E, list(T)).
type(Context, cons(T, E1, E2), list(T)) :- type(Context, E1, T),
										   type(Context, E2, list(T)).
type(Context, head(T, E), T) :- type(Context, E, list(T)).
type(Context, tail(T, E), list(T)) :- type(Context, E, list(T)).
