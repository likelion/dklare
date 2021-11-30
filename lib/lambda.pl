/*
Copyright 2021 Leonid Mokrushin

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
*/

:- module(lambda, [ op(1200, xfx, ===),
                    op(1100, xfx, where),
                    op(1100, xfx, if),
                    op(600, yfx, @),
                    '@'/3,
                    eval/2 ] ).

:- discontiguous '@'/3.
:- dynamic fun/1, '@'/3.

eval(G, R) :-
  prolog_load_context(module, Mo),
  expand_expression(G, true, T, [], L, R),
  Mo:maplist(assertz, L),
  Mo:T,
  maplist(abolishl(Mo), L).

abolishl(Mo, H:-_) :-
  functor(H, F, A),
  abolish(Mo:F/A).

user:term_expansion(H === B, Clauses) :-
  strip_module(H, M, P),
  ( M == lambda
  -> prolog_load_context(module, Mo)
  ; Mo = M
  ),
  P =.. [F|A0],
  length(A0, A),
  define(Mo:F/A, PA),
  append(PA, [H2:-B2|L], Clauses),
  append(A0, [R], A1),
  H2 =.. [F|A1],
  phrase(expand_body(B, B2, R), L),
  succ(A, A2),
  export(Mo:F/A2).

define(T, []) :-
  fun(T), !.
define(T, PA) :-
  assertz(fun(T)),
  partial_application_expand(T, PA).

partial_application_expand(Mo:F/Ar0, PA) :-
  Ar is Ar0 - 1,
  length(A, Ar),
  XX =.. [F|A],
  append(A, [X,Y], A2),
  XXX =.. [F|A2],
  append(PA0, [lambda:'@'(XX,X,Y):-XXX], PA),
  partial_application_expand_(Mo:F/Ar, PA0).

partial_application_expand_(_:_/0, []) :- !.
partial_application_expand_(Mo:F/Ar0, PA) :-
  Ar is Ar0 - 1,
  length(A, Ar),
  XX =.. [F|A],
  append(A, [X], A2),
  XXX =.. [F|A2],
  append(PA0, [lambda:'@'(XX,X,XXX)], PA),
  partial_application_expand_(Mo:F/Ar, PA0).

expand_body(B, !, B) -->
  { var(B) }, !.
expand_body(X if Y, B, R) --> !,
  expand_condition(Y, Y1),
  { conj(Y1, !, Y2) },
  expand_expression(X, Y2, B, R).
expand_body(X where Y, B, R) --> !,
  expand_condition(Y, Y1),
  expand_expression(X, Y1, B, R).
expand_body(B0, B, R) -->
  expand_expression(B0, !, B, R).

conj(A, true, A) :- !.
conj(true, B, B) :- !.
conj(A, B, (A,B)).

expand_condition(X, X) -->
  { var(X) }, !.
expand_condition((X,Y), C) --> !,
  expand_condition(X, C1),
  expand_condition(Y, C2),
  { conj(C1, C2, C) }.
expand_condition(X=Y, C) --> !,
  expand_expression(X, true, C1, RX),
  expand_expression(Y, C1, C2, RY),
  { conj(C2, (RX=RY), C) }.
expand_condition(X, X) --> [].

expand_expression(X, E, E, X) -->
  { var(X) }, !.
expand_expression([A|B], E0, E, [RA|RB]) --> !,
  expand_expression(A, E0, E1, RA),
  expand_expression(B, E1, E, RB).
expand_expression(X, E0, E, R) -->
  { callable(X),
    strip_module(X, M, P),
    P =.. [XF|A0]
  },
  ( { XF == f }
  -> { decompose_lambda(A0, Body, [], Bound, Free),
       gensym('_f', RF),
       R =.. [RF|Free],
       E = E0,
       append(Free, Bound, Args0),
       append(Args0, [R1], Args),
       Def =.. [RF|Args]
     },
     expand_expression(Body, true, Body2, R1),
     [Def :- Body2],
     { ( M == lambda
       -> prolog_load_context(module, Mo)
       ; Mo = M
       ),
       length(Args0, Ar),
       define(Mo:RF/Ar, PA)
     },
     [PA]
  ; { ( XF == '@',
        length(A0, 2)
      ; ( M == lambda
        -> true
        ; Mo = M
        ),
        length(A0, A),
        fun(Mo:XF/A)
      ),
      append(A0, [R], A1)
    },
    expand_arguments(A1, A2, E0, E1, R),
    { E2 =.. [XF|A2],
      conj(E1, E2, E)
    }
  ), !.
expand_expression(X, E, E, X) --> [].

decompose_lambda([Ta], Ta, Bo0, Bo, Fr) :- !,
  term_variables(Ta, F0),
  without(F0, Bo0, Fr),
  reverse(Bo0, Bo).
decompose_lambda([H|T], Ta, L0, B, F) :-
  var(H),
  decompose_lambda(T, Ta, [H|L0], B, F).

without([], _, []) :- !.
without(S, [], S) :- !.
without(S, [H|T], R) :-
  exclude(==(H),S,S1),
  without(S1, T, R).

expand_arguments([R], [R], G, G, R) --> !.
expand_arguments([H|T], [H2|T2], G0, G, R) -->
  expand_expression(H, G0, G1, H2),
  expand_arguments(T, T2, G1, G, R).
