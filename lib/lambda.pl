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
                    op(600, xfx, @),
                    term_expansion/2 ] ).

:- dynamic fun/1.

term_expansion(H === B, H2 :- B2) :-
  H =.. [F|A0],
  length(A0, A),
  define(F/A),
  append(A0, [R], A1),
  H2 =.. [F|A1],
  expand_body(B, B2, R).

define(T) :-
  fun(T), !.
define(T) :-
  assertz(fun(T)).

expand_body(A, true, A) :-
  var(A), !.
expand_body(A where B, A2, R) :- !,
  fp_expand(A, B, A2, R).
expand_body(A, A2, R) :-
  fp_expand(A, true, A2, R).

fp_expand(X, Y, Y, X) :-
  var(X), !.
fp_expand([A|B], Y0, Y, [RA|RB]) :- !,
  fp_expand(A, Y0, Y1, RA),
  fp_expand(B, Y1, Y, RB).
fp_expand(X, Y0, Y, R) :-
  callable(X),
  X =.. [F|A0],
  length(A0, A),
  fun(F/A), !,
  append(A0, [R], A1),
  expand_f(A1, A2, Y0, Y1, R),
  YY =.. [F|A2],
  conj(Y1, YY, Y).
fp_expand(X, Y, Y, X).

conj(A, true, A) :- !.
conj(true, B, B) :- !.
conj(A, B, (A,B)).

expand_f([R], [R], G0, G0, R) :- !.
expand_f([H|T], [H2|T2], G0, G, R) :-
  fp_expand(H, G0, G1, H2),
  expand_f(T, T2, G1, G, R).
