:- module(prelude, []).

:- reexport(library(lambda)).

'@'(A,B) === R where
  functor(A, F, Ar),
  succ(Ar, Ar2),
  ( lambda:fun(M:F/Ar2)
  -> apply(M:A,[B,R])
  ; A =.. [F|A0],
    append(A0, [B], A1),
    R =.. [F|A1]
  ).

+(X,Y) === Z where Z is X+Y.
-(X,Y) === Z where Z is X-Y.
*(X,Y) === Z where Z is X*Y.
/(X,Y) === Z where Z is X/Y.

factorial(0) === 1 where !.
factorial(N) === N*factorial(N-1) where N > 0.

map(_,[]) === [] where !.
map(F,[X|Xs]) === [F@X|map(F,Xs)].

not(true) === false.
not(false) === true.

neg(L) === map(not,L).

>(X,Y) === true where X>Y, !.
>(_,_) === false.

filter(_,[]) === [] where !.
filter(P,[X|Xs]) === [X|filter(P,Xs)] where P@X=true, !.
filter(P,[_|Xs]) === filter(P,Xs).

foldr(_,[],E) === E where !.
foldr(F,[X|Xs],E) === (F@X)@(foldr(F,Xs,E)).

foldl(_,[],E) === E where !.
foldl(F,[X|Xs],E) === foldl(F,Xs,(F@E)@X).

zip([],_) === [] where !.
zip(_,[]) === [] where !.
zip([X|Xs],[Y|Ys]) === [(X,Y)|zip(Xs,Ys)].
