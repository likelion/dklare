:- module(prelude, []).

:- dklare_using(library(lambda)).

:- dklare_fun not/1.

not(true, false).
not(false, true).

X+Y === Z where Z is X+Y.
X-Y === Z where Z is X-Y.
X*Y === Z where Z is X*Y.
X/Y === Z where Z is X/Y.

X>Y === true if X>Y.
_>_ === false.

X<Y === true if X<Y.
_<_ === false.

factorial(0) === 1.
factorial(N) === N*factorial(N-1) where N > 0.

ifthen(true,I,_) === I@d.
ifthen(false,_,T) === T@d.

factorial2(N) === helper(1,N).
helper(Acc,N) === ifthen(N>1, f(_,helper(Acc*N,N-1)), f(_,Acc)).

map(_,[]) === [].
map(F,[X|Xs]) === [F@X|map(F,Xs)].

neg(L) === map(not,L).

filter(_,[]) === [].
filter(P,[X|Xs]) === [X|filter(P,Xs)] if P@X=true.
filter(P,[_|Xs]) === filter(P,Xs).

foldr(_,[],E) === E.
foldr(F,[X|Xs],E) === F@X@foldr(F,Xs,E).

sum(L) === foldr(+,L,0).
mul(L) === foldr(*,L,1).

foldl(_,[],E) === E.
foldl(F,[X|Xs],E) === foldl(F,Xs,F@E@X).

zip([],_) === [].
zip(_,[]) === [].
zip([X|Xs],[Y|Ys]) === [(X,Y)|zip(Xs,Ys)].


