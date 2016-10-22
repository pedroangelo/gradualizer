%% Extension to Simply Typed Lambda Calculus type system
%% General Recursion (fixed point operator)

type(Context, fix(E), T) :- type(Context, E, arrow(T, T)).
