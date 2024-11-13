:- module(types, [ infer_type/6,
                   process/3,
                   op(500, yfx, $),
                   op(550, xfy, ~>)
                 ] ).

kind(X, K, KC)    :- look(X:K, KC).
kind(F$G, K, KC)  :- kind(F, K0~>K, KC),
                     kind(G, K0, KC).
kind(A~>B, o, KC) :- kind(A, o, KC),
                     kind(B, o, KC).

look(X:poly(A, []), _) :-
  integer(X), !,
  put_attr(A,types,[num]).
look(X:poly(A, []), _) :-
  float(X), !,
  put_attr(A,types,[fractional]).
look(X:T, [X1:T1|Xs]) :-
  ( unify_with_occurs_check(X, X1),
    unify_with_occurs_check(T, T1)
  -> true
  ; look(X:T, Xs)
  ).

type(E, T, C) :-
  call_dcg(type(E, T, C), _, _).

type(var(X), T, C)       --> { look(X:T0, C) },
                             inst_type(T0, T1),
                             { unify_with_occurs_check(T1, T) }.
type(lam(X,E), A~>B, C)  --> type(E, B, [X:mono(A)|C]),
                             [ kind(A~>B, o) ].
type(X$Y, B, C)          --> type(X, A~>B, C),
                             type(Y, A0, C),
                             { unify_with_occurs_check(A, A0) }.
type(let(X=E0,E1), T, C) --> type(E0, A, C),
                             { term_variables(C, Vs) },
                             type(E1, T, [X:poly(A,Vs)|C]).

inst_type(T, T)          --> { var(T) }, !.
inst_type(mono(T), T)    --> [].
inst_type(poly(T0,V), T) --> { copy_term(t(T0,V), t(T,V)),
                               term_variables(T0, Xs0),
                               term_variables(T, Xs)
                             },
                             samekinds(Xs0, Xs).

samekinds([], [])         --> [].
samekinds([X|Xs], [Y|Ys]) --> [ kind(X, K), kind(Y, K) ],
                              samekinds(Xs, Ys).

infer_type(E, T, C, KC, K, CT) :-
  phrase(type(E, T, C), CHs),
  format('typechecked~n',[]),
  get_types(CHs, Ts),
  term_variables(Ts, Vs),
  reset_gensym(t),
  maplist(variablize, Vs, KC1, CT),
  append(KC1, KC, K),
  maplist(check_kind(K), CHs).

get_types([], []).
get_types([kind(Type,_)|T1], [Type|T2]) :-
  get_types(T1, T2).

variablize(X, X:_, X:C) :- ignore(get_attr(X, types, C)), del_attrs(X), gensym(t, X).

check_kind(KC, K) :-
  call(K, KC).

class(eq, []).
class(show, []).
class(ord, [eq]).
class(num, [eq, show]).
class(fractional, [num]).

process(T, K, CT):-
  KC0 = [ 'Nat'   : o
        , 'Int'   : o
        , 'Float' : o
        , 'Bool'  : o
        , 'Num'   : o~>o
        , 'List'  : o~>o
        , 'Maybe' : o~>o
        ],
  C0 = [ 'Zero' : mono('Nat')
       , 'Succ' : mono('Nat'~>'Nat')
       , 'Nil'  : poly('List'$A, [])
       , 'Cons' : poly(A~>'List'$A~>'List'$A, [])
       , 'None' : poly('Maybe'$B, [])
       , 'Just' : poly(B~>'Maybe'$B, [])
       , 'True' : mono('Bool')
       , 'False' : mono('Bool')
       ],
  infer_type(lam(X,lam(L,var('Cons')$var(X)$var(L)))$var(3.0)$(var('Cons')$var(1)$var('Nil')), T, C0, KC0, K, CT).
 %infer_type(var('Cons')$(var('Just')$var(1))$var('Nil'), T, C0, KC0, K, CT).

% An alternative to the use of type classes is to pass the functions
% that belong to the type class as extra parameters. Indeed, that’s
% exactly how type classes are implemented in practice.

attr_unify_hook(Attr, Value) :-
  format('check that ~w has classes ~w~n', [Value, Attr]),
  ( var(Value)
  -> put_attr(Value, types, Attr)
  ; true
  ).
