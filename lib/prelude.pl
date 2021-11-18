:- module(prelude, []).

:- reexport(library(lambda)).

@(A,B) === R where
  functor(A, F, Ar),
  succ(Ar, Ar2),
  ( lambda:fun(M:F/Ar2)
  -> apply(M:A,[B,R])
  ; A =.. [F|A0],
    append(A0, [B], A1),
    R =.. [F|A1]
  ).

X+Y === Z where Z is X+Y.
X-Y === Z where Z is X-Y.
X*Y === Z where Z is X*Y.
X/Y === Z where Z is X/Y.

factorial(0) === 1.
factorial(N) === N*factorial(N-1) where N > 0.

map(_,[]) === [].
map(F,[X|Xs]) === [F@X|map(F,Xs)].

not(true) === false.
not(false) === true.

neg(L) === map(not,L).

X>Y === true if X>Y.
_>_ === false.

X<Y === true if X<Y.
_<_ === false.

filter(_,[]) === [].
filter(P,[X|Xs]) === [X|filter(P,Xs)] if P@X=true.
filter(P,[_|Xs]) === filter(P,Xs).

foldr(_,[],E) === E.
foldr(F,[X|Xs],E) === (F@X)@(foldr(F,Xs,E)).

foldl(_,[],E) === E.
foldl(F,[X|Xs],E) === foldl(F,Xs,(F@E)@X).

zip([],_) === [].
zip(_,[]) === [].
zip([X|Xs],[Y|Ys]) === [(X,Y)|zip(Xs,Ys)].
