:- module(prelude, []).

:- use_module(library(lambda)).

'@'(A,B) === R where apply(A,[B,R]).

*(X,Y) === Z where Z is X*Y.
-(X,Y) === Z where Z is X-Y.

factorial(0) === 1 where !.
factorial(N) === N*factorial(N-1) where N > 0.

map(_,[]) === [] where !.
map(F,[X|Xs]) === [F@X|map(F,Xs)].

not(true) === false.
not(false) === true.

neg(L) === map(not,L).
