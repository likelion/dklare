
:- use_module(reif).

:- op(500, yfx, $).

type(var(X),      T, C) :- atom(X), context_var_type(C, X, mono(T), true).
type(lam(X,E), A->B, C) :- type(E, B, [X:mono(A)|C]).
type(X $ Y,       B, C) :- type(X, A->B, C), type(Y, A, C).

context_var_type([], _, _, false).
context_var_type([V:T0|VTs], V, T, R) :-
  if_(
    unify_with_occurs_check(T0, T),
    R = true,
    context_var_type(VTs, V, T, R)
  ).


% ?- type(lam(x,lam(y,var(x))), T, []).
% T = '$VAR'('_A')->'$VAR'('_')->'$VAR'('_A').

% ?- type(lam(f,lam(y,lam(x,var(f)$var(x)$var(y))))$lam(i,var(i)), T, []).
% T = '$VAR'('_A')->('$VAR'('_A')->'$VAR'('_B'))->'$VAR'('_B').

%:- set_prolog_flag(occurs_check,true).

% type0(var(X),      T, C) :- first(X:mono(T), C).
% type0(lam(X,E), A->B, C) :- type0(E, B, [X:mono(A)|C]).
% type0(X $ Y,       B, C) :- type0(X, A->B, C), type0(Y, A, C).

% first(K:V, [K:V1|_]) :- unify_with_occurs_check(V, V1).
% first(K:V, [_|Xs]) :- first(K:V, Xs).