:- op(500, yfx, $).

kind(var(Z), K, KC) :- look(Z:K0, KC), instantiate(K0, K).
kind(F$G,    K, KC) :- kind(F, K0->K, KC), kind(G, K0, KC).
kind(A->B,   o, KC) :- kind(A, o, KC), kind(B, o, KC).

look((X:Y), Xs) :- memberchk((X:Y0), Xs), unify_with_occurs_check(Y0, Y).

instantiate(mono(T), T) :- !.
instantiate(poly(T0,C), T) :- copy_term(t(C,T0), t(C,T)).

type(var(X), T, C, KC) -->
  { format('~@look {~w : ~w}~n~@CO=~w~n~@KC=~w~n', [indent,X,T,indent,C,indent,KC]) },
  ( { look(X:T0, C) } ,
      inst_type(T0, T, KC)
  *-> { format('~@foun {~w : ~w}~n', [indent,X,T]) }
  ; { format('~@~w not found~n', [indent,X]), fail }
  ).

type(lam(X,E), A->B, C, KC) -->
  { format('~@?-- lam(~w, ~w) : ~w~n~@CO=~w~n~@KC=~w~n', [indent,X,E,A->B,indent,C,indent,KC]), add_indent },
  ( type(E, B, [X:mono(A)|C], KC),
    [kind(A->B, o, KC)],
    {format('~@kind(~w, o)~n', [indent,A->B])}
  *-> { del_indent, format('~@--> lam(~w, ~w) : ~w~n', [indent,X,E,A->B]) }
  ; { del_indent, format('~@###~n', [indent]), fail }
  ).

type(X$Y, B, C, KC) -->
  { format('~@?-- ~w $ ~w : ~w~n~@CO=~w~n~@KC=~w~n', [indent,X,Y,B,indent,C,indent,KC]), add_indent },
  ( type(X, A->B, C, KC), type(Y, A, C, KC)
  *-> { del_indent, format('~@--> ~w $ ~w : ~w~n', [indent,X,Y,B]) }
  ; { del_indent, format('~@###~n', [indent]), fail }
  ).

type(let(X=E0,E1), T, C, KC) -->
  { format('~@?-- let(~w : ~w, ~w : ~w)~n~@CO=~w~n~@KC=~w~n', [indent,E0,A,E1,T,indent,C,indent,KC]), add_indent },
  ( type(E0, A, [X:mono(A)|C], KC),
    { term_variables(C, Vs) },
    type(E1, T, [X:poly(A,Vs)|C], KC)
  *-> { del_indent, format('~@--> let(~w : ~w, ~w : ~w)~n', [indent,E0,A,E1,T]) }
  ; { del_indent, format('~@###~n', [indent]), fail }
  ).

inst_type(poly(T0,C), T, KC) --> { copy_term(t(T0,C), t(T1,C)),
                                   term_variables(T0, Xs0),
                                   term_variables(T1, Xs1)
                                 },
                                 samekinds(Xs0, Xs1, KC),
                                 { T = T1 }.
inst_type(mono(T), T, _) --> [].

samekinds([], [], _) --> [].
samekinds([X|Xs], [Y|Ys], KC) --> [kind(X, K, KC), kind(Y, K, KC)], samekinds(Xs, Ys, KC).

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

t(E, T, C, KC) :-
  retractall(indent(_)),

  phrase(type(E, T, C, KC), Gs0),
  copy_term(Gs0, Gs),
  get_types(Gs, Tys),
  term_variables(Tys, Xs),
  maplist(variablize, Xs, KC1),
  appendKC(Gs, KC1, Gs1),
  print_term(Gs1, []),
  maplist(call, Gs1),
  print_term(Gs1, []),

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
  reset_gensym(a),
  reset_gensym(b),
  reset_gensym(t), nl.

get_types([], []).
get_types([kind(Type,_,_)|T1], [Type|T2]) :-
  get_types(T1, T2).

variablize(var(X), X:_) :- gensym(t, X).

appendKC([], _, []) :- !.
appendKC([kind(X, K, KC)|Gs], KC1, [kind(X, K, KC2)|Gs1]) :-
  append(KC1, KC, KC2),
  appendKC(Gs, KC1, Gs1).

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

process(T):-
  KC0 = [ 'Nat' :mono(o)
        , 'List':mono(o->o)
        ],
  Nat = var('Nat'),
  List = var('List'),
  C0 = [ 'Zero':mono(Nat)
       , 'Succ':mono(Nat -> Nat)
       , 'Nil' :poly(List$A, [])
       , 'Cons':poly(A->List$A->List$A, [])
       ],
  t(lam(x,lam(l,var('Cons')$var(x)$var(l))), T, C0, KC0).