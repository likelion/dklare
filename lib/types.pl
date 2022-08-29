:- module(types, [ infer_type/5,
                   process/2,
                   op(500, yfx, $),
                   op(550, xfy, ->)
                 ] ).

kind(X, K, KC)    :- look(X:K, KC).
kind(F$G, K, KC)  :- kind(F, K0->K, KC),
                     kind(G, K0, KC).
kind(A->B, o, KC) :- kind(A, o, KC),
                     kind(B, o, KC).

look(X:Y, [X1:Y1|Xs]) :-
  ( (X == X1)
  -> unify_with_occurs_check(Y, Y1)
  ; look(X:Y, Xs)
  ).

type(var(X), T, C)       --> { look(X:T0, C) },
                             inst_type(T0, T1),
                             { unify_with_occurs_check(T1, T) }.
type(lam(X,E), A->B, C)  --> type(E, B, [X:mono(A)|C]),
                             [ kind(A->B, o) ].
type(X$Y, B, C)          --> type(X, A->B, C),
                             type(Y, A0, C),
                             { unify_with_occurs_check(A0, A) }.
type(let(X=E0,E1), T, C) --> type(E0, A, C),
                             { term_variables(C, Vs) },
                             type(E1, T, [X:poly(A,Vs)|C]).

inst_type(mono(T), T)    --> [].
inst_type(poly(T0,V), T) --> { copy_term(t(T0,V), t(T,V)),
                               term_variables(T0, Xs0),
                               term_variables(T, Xs)
                             },
                             samekinds(Xs0, Xs).

samekinds([], [])         --> [].
samekinds([X|Xs], [Y|Ys]) --> [ kind(X, K), kind(Y, K) ],
                              samekinds(Xs, Ys).

infer_type(E, T, C, KC, K) :-
  phrase(type(E, T, C), CHs),
  get_types(CHs, Ts),
  term_variables(Ts, Vs),
  reset_gensym(t),
  maplist(variablize, Vs, KC1),
  append(KC1, KC, K),
  maplist(check_kind(K), CHs).

get_types([], []).
get_types([kind(Type,_)|T1], [Type|T2]) :-
  get_types(T1, T2).

variablize(X, X:_) :- gensym(t, X).

check_kind(KC, K) :-
  call(K, KC).

process(T, K):-
  KC0 = [ 'Nat' : o
        , 'List': o->o
        ],
  Cons = var('Cons'),
  C0 = [ 'Zero' : mono('Nat')
       , 'Succ' : mono('Nat'->'Nat')
       , 'Nil'  : poly('List'$A, [])
       , 'Cons' : poly(A->'List'$A->'List'$A, [])
       ],
  infer_type(lam(X,lam(L,Cons$var(X)$var(L))), T, C0, KC0, K).
  %infer_type('Cons'$'Zero'$('Cons'$'Zero'$'Nil'), T, C0, KC0, K).

