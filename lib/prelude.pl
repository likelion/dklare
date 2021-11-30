:- module(prelude, []).

:- reexport(library(lambda)).

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
foldr(F,[X|Xs],E) === F@X@foldr(F,Xs,E).

foldl(_,[],E) === E.
foldl(F,[X|Xs],E) === foldl(F,Xs,F@E@X).

zip([],_) === [].
zip(_,[]) === [].
zip([X|Xs],[Y|Ys]) === [(X,Y)|zip(Xs,Ys)].

ifthen(true,I,_) === I@d.
ifthen(false,_,T) === T@d.

helper(Acc,N) === ifthen(N>1, f(_,helper(Acc*N,N-1)), f(_,Acc)).
factorial2(N) === helper(1,N).

