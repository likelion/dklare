:- module(simple_types, [ op(500, yfx, $),
                          op(550, xfy, ~>)
                        ] ).

look(X:T, [X1:T1|Xs]) :-
  ( X = X1,
    acyclic_term(X),
    T = T1,
    acyclic_term(T)
  -> true
  ; look(X:T, Xs)
  ).

type(var(X), T, C)      :- look(X:T, C).

type(lam(X,E), A~>B, C) :- type(E, B, [X:A|C]),
                           acyclic_term(E),
                           acyclic_term(B),
                           acyclic_term(X),
                           acyclic_term(A).

type(X$Y, B, C)         :- type(X, A~>B, C),
                           acyclic_term(X),
                           acyclic_term(B),
                           type(Y, A, C),
                           acyclic_term(Y),
                           acyclic_term(A).
