:- use_module(reif).

:- op(500, yfx, $).

type(var(X),     T1, C) :-
  format('~@look {~w : ~w}       in ~w~n', [indent,X,T1,C]),
  ( look(X:T, C), instantiate(T, T1)
  *-> format('~@     {~w : ~w} found in ~w~n', [indent,X,T1,C])
  ; format('~@~w not found~n', [indent,X]),
    fail
  ).

type(lam(X,E), A->B, C) :-
  format('~@?-- lam(~w, ~w) : ~w~n', [indent,X,E,A->B]),
  add_indent,
  ( type(E, B, [X:mono(A)|C])
  *-> del_indent,
     format('~@--> lam(~w, ~w) : ~w~n', [indent,X,E,A->B])
  ; del_indent,
    format('~@###~n', [indent]),
    fail
  ).

type(X $ Y,       B, C) :-
  format('~@?-- ~w $ ~w : ~w~n', [indent,X,Y,B]),
  add_indent,
  ( type(X, A->B, C), type(Y, A, C)
  *-> del_indent,
     format('~@--> ~w $ ~w : ~w~n', [indent,X,Y,B])
  ; del_indent,
    format('~@###~n', [indent]),
    fail
  ).

type(let(X=E0,E1), T, C) :-
     format('~@?-- let(~w : ~w, ~w : ~w)~n', [indent,E0,A,E1,T]),
  add_indent,
  ( type(E0, A, [X:mono(A)|C]), type(E1, T, [X:poly(A,C)|C])
  *-> del_indent,
     format('~@--> let(~w : ~w, ~w : ~w)~n', [indent,E0,A,E1,T])
  ; del_indent,
    format('~@###~n', [indent]),
    fail
  ).

instantiate(mono(T), T) :- !.
instantiate(poly(T,C), T1) :- copy_term(t(C,T), t(C,T1)).

look(V, [V1|Xs]) :-
  if_(
    unify_with_occurs_check(V, V1),
    true,
    look(V, Xs)
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

t(E, T, C) :-
  retractall(indent(_)),
  type(E, T, C),
  '$skip_list'(_, C, Tail),
  once((
    var(Tail),
    Tail = []
  ; true)),
  name_free_vars(E, t(E,T,C), x),
  name_free_vars(T, t(E,T,C), a),
  name_free_vars(C, t(E,T,C), b),
  term_variables(t(E,T,C), Vars),
  maplist(=('_'), Vars),
  reset_gensym(x),
  reset_gensym(a), nl.

name_free_vars(Term, Term2, Prefix) :-
  term_variables(Term, Vars),
  term_singletons(Term2, Single),
  convlist(not_from(Single), Vars, List),
  maplist(gensym(Prefix), List).

not_from(List, Elem, Elem) :-
  \+ contains_var(Elem, List).

:- dynamic indent/1.

add_indent :-
  ignore((
    ( indent(I)
    -> retractall(indent(_)),
       atom_concat(' ', I, NI)
    ; NI = ' '
    ),
    assertz(indent(NI))
  )).

del_indent :-
  ignore((
    ( indent(I)
    -> retractall(indent(_)),
       ( sub_atom(I, 1, _, 0, NI)
       -> ( NI == ''
          -> true
          ; assertz(indent(NI))
          )
       ; true
       )
    ; true
    )
  )).

indent :-
  ( indent(I_)
  -> I = I_
  ; I = ''
  ),
  format(current_output, '~w', [I]).