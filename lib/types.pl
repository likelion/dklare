:- module(types, [ type/3,
                   op(500, yfx, $) ] ).

type(var(X), T, C) :- first(X:T0, C), instantiate(T0, T).
type(lam(X,E), A->B, C) :- type(E, B, [X:mono(A)|C]).
type(X$Y, B, C) :- type(X, A->B, C), type(Y, A, C).
type(let(X=E0,E1), T, C) :- type(E0, A, C), type(E1, T, [X:poly(C,A)|C]).

first(K:V, [K:V1|_]) :- unify_with_occurs_check(V, V1), !.
first(K:V, [_|Xs]) :- first(K:V, Xs).

instantiate(poly(C,T0), T) :- copy_term(t(C,T0), t(C,T)).
instantiate(mono(T), T).
