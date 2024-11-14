:- module(types, [ type/2,
                   test/0,
                   op(500, yfx, $),
                   op(550, xfy, ~>)
                 ] ).

look(X:T, [X1:T1|Xs]) -->
  ( { X = X1 },
    { acyclic_term(X) },
    ( { T1 = mono(T) }
    -> []
    ; { T1 = poly(T0, C) }
    -> { copy_term(t(T0,C), t(T2,C)),
         term_variables(T0, Xs0),
         term_variables(T2, Xs2)
       },
       samekinds(Xs0, Xs2),
       { T = T2 }
    ),
    { acyclic_term(T) },
    { acyclic_term(T1) }
  -> []
  ; look(X:T, Xs)
  ).

type(var(X), T, C)      --> look(X:T, C).
                            % { format("TYPE: ~w :: ~w~n", [X,T]) }.
type(lam(X,E), A~>B, C) --> type(E, B, [X:mono(A)|C]),
                            { acyclic_term(E) },
                            { acyclic_term(B) },
                            { acyclic_term(X) },
                            { acyclic_term(A) },
                            %{ format("TYPE: ~w :: ~w~n", [X,mono(A)]) },
                            [ kind(A~>B, o) ].
type(X$Y, B, C)         --> type(X, A~>B, C),
                            { acyclic_term(X) },
                            { acyclic_term(B) },
                            type(Y, A, C),
                            { acyclic_term(Y) },
                            { acyclic_term(A) }.
                            %{ format("TYPE: ~w :: ~w~n", [X,A~>B]) },
                            %{ format("TYPE: ~w :: ~w~n", [Y,A]) }.
type(let(X=E0,E), T, C) --> type(E0, A, C),
                            { acyclic_term(E0) },
                            type(E, T, [X:poly(A,C)|C]),
                            { acyclic_term(E) },
                            { acyclic_term(T) },
                            { acyclic_term(X) },
                            { acyclic_term(A) }.
                            %{ format("TYPE: ~w :: ~w~n", [X,poly(A)]) }.

samekinds([], []) --> [].
samekinds([X|Xs], [Y|Ys]) -->
  ( { X\==Y }
  -> [ kind(X, K), kind(Y, K) ]
  ; []
  ),
  samekinds(Xs, Ys).

look(X:T, [X1:T1|Xs]) :-
  ( X = X1,
    acyclic_term(X),
    ( T1 = mono(T)
    -> true
    ; T1 = poly(T, C)
    -> copy_term(t(T1,C), t(T,C))
    ),
    acyclic_term(T),
    acyclic_term(T1)
  -> true
  ; look(X:T, Xs)
  ).

kind(var(X), K, KC) :- look(X:K, KC).
kind(F$G, K, KC)    :- kind(F, K0~>K, KC),
                       kind(G, K0, KC).
kind(A~>B, o, KC)   :- kind(A, o, KC),
                       kind(B, o, KC).

% ---------

type(E, T) :-
  KC0 = [ 'Nat'   : mono(o)
        , 'Int'   : mono(o)
        , 'Float' : mono(o)
        , 'Bool'  : mono(o)
        , 'Num'   : mono(o~>o)
        , 'List'  : mono(o~>o)
        , 'Maybe' : mono(o~>o)
        |_],
  Nat = var('Nat'),
  List = var('List'),
  Maybe = var('Maybe'),
  Bool = var('Bool'),
  C0 = [ 'Zero'  : mono(Nat)
       , 'Succ'  : mono(Nat~>Nat)
       , 'Nil'   : poly(List$A, [])
       , 'Cons'  : poly(A~>List$A~>List$A, [])
       , 'None'  : poly(Maybe$B, [])
       , 'Just'  : poly(B~>Maybe$B, [])
       , 'True'  : mono(Bool)
       , 'False' : mono(Bool)
       ],
  ( var(E)->true;var_exp(E0, E) ),
  ( var(T)->true;var_exp(T0, T) ),
  call_dcg(type(E0, T0, C0), CHs, []),
  ( var(T)->copy_term(T0, T1),var_exp(T1,T),namevars(T, t);true ),
  get_types(CHs, Ts),
  term_variables(Ts, Vs),
  maplist(variablize, Vs),
  maplist(check_kind(KC0), CHs),
  ( var(E)->var_exp(E0, E),namevars(E, x);true ),
  format("~w : ~w~n", [E,T]).

var_exp(var(E), E) :-
  \+ compound(E), !.
var_exp(lam(V1,V2), lam(V1,E2)) :- !,
  var_exp(V2,E2).
var_exp(V1$V2, E1$E2) :- !,
  var_exp(V1,E1),
  var_exp(V2,E2).
var_exp(let(V1=V2,V3), let(V1=E2,E3)) :- !,
  var_exp(V2,E2),
  var_exp(V3,E3).
var_exp(V1~>V2, E1~>E2) :- !,
  var_exp(V1,E1),
  var_exp(V2,E2).

get_types([], []).
get_types([kind(Type,_)|T1], [Type|T2]) :-
  get_types(T1, T2).

variablize(var(X)) :-
  gensym(t,X).

check_kind(KC, kind(E, K)) :-
  kind(E, K, KC).

namevars(T, L) :-
  term_variables(T, Vs),
  reset_gensym(L),
  namevar(Vs, L, T).

namevar([], _, _) :- !.
namevar([V|VT], L, T) :-
  gensym(L, V0),
  \+ contains_var(V0, T),
  V = V0,
  namevar(VT, L, T).

test:-
  type(lam(x,x), _), nl,                      % t1~>t1
  type(lam(x,lam(y,x$y)), _), nl,             % (t1~>t2)~>t1~>t2
  type(lam(x,lam(y,y$x)), _), nl,             % t1~>(t1~>t2)~>t2
  type(let(id=lam(x,x),id$id), _), nl,        % t1~>t1
  type(lam(x,'Zero'), _), nl,                 % t1~>Nat
  type(lam(x,lam(n,x$'Succ'$n)), _), nl,      % ((Nat~>Nat)~>t1~>t2)~>t1~>t2
  type(lam(x,'Cons'$x$('Cons'$x$'Nil')), _).  % t1~>List$t1
