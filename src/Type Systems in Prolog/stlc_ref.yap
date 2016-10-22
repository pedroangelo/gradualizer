%% Extension to Simply Typed Lambda Calculus type system
%% References

:- [stlc_unit].

type(Context, ref(E), refType(T)) :- type(Context, E, T).
type(Context, deref(E), T) :- type(Context, E, refType(T)).
type(Context, assign(E1, E2), unitType) :- type(Context, E1, refType(T)),
										   type(Context, E2, T).
