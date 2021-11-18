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
                    op(600, xfx, @),
                    eval/2 ] ).

:- dynamic fun/1.

eval(G, R) :-
  f_expand(G, true, T, R),
  call(T).

user:term_expansion(H === B, H2 :- B2) :-
  strip_module(H, M, P),
  ( M == lambda
  -> prolog_load_context(module, Mo)
  ; Mo = M
  ),
  P =.. [F|A0],
  length(A0, A),
  define(Mo:F/A),
  append(A0, [R], A1),
  H2 =.. [F|A1],
  ( var(B)
  -> R = B,
     B2 = !
  ; ( B = (X if Y)
    -> b_expand(Y, Y1),
       conj(Y1, !, Y2),
       f_expand(X, Y2, B2, R)
    ; B = (X where Y)
    -> b_expand(Y, Y1),
       f_expand(X, Y1, B2, R)
    ; f_expand(B, !, B2, R)
    )
  ),
  succ(A, A2),
  export(Mo:F/A2).

define(T) :-
  fun(T), !.
define(T) :-
  assertz(fun(T)).

conj(A, true, A) :- !.
conj(true, B, B) :- !.
conj(A, B, (A,B)).

b_expand(X, X) :-
  var(X), !.
b_expand((X,Y), B) :- !,
  b_expand(X, B1),
  b_expand(Y, B2),
  conj(B1, B2, B).
b_expand(X=Y, B) :- !,
  f_expand(X, true, B1, RX),
  f_expand(Y, B1, B2, RY),
  conj(B2, (RX=RY), B).
b_expand(X, X).

f_expand(X, Y, Y, X) :-
  var(X), !.
f_expand([A|B], Y0, Y, [RA|RB]) :- !,
  f_expand(A, Y0, Y1, RA),
  f_expand(B, Y1, Y, RB).
f_expand(X, Y0, Y, R) :-
  callable(X),
  strip_module(X, M, P),
  ( M == lambda
  -> true
  ; Mo = M
  ),
  P =.. [F|A0],
  length(A0, A),
  fun(Mo:F/A), !,
  append(A0, [R], A1),
  a_expand(A1, A2, Y0, Y1, R),
  YY =.. [F|A2],
  conj(Y1, YY, Y).
f_expand(X, Y, Y, X).

a_expand([R], [R], G0, G0, R) :- !.
a_expand([H|T], [H2|T2], G0, G, R) :-
  f_expand(H, G0, G1, H2),
  a_expand(T, T2, G1, G, R).
