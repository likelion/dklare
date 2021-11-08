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

:- module(lambda, []).

:- op(1200, xfx, ===).
:- op(1100, xfx, where).

:- dynamic def/2.
:- style_check([-discontiguous]).

term_expansion(H === B, Result) :-
  H =.. [F|FA],
  atom_concat('_', F, F2),
  same_length(FA, EA),
  HT =.. [F|EA],
  append(EA, [_], EA2),
  HT2 =.. [F2|EA2],
  ( def(HT,HT2)
  -> Result = Result0
  ; Result = [def(HT,HT2)|Result0]
  ),
  Result0 = [H2:-B2],
  expand_body(B, BE, R),
  to_conj(BE,B2),
  append(FA, [R], FA2),
  H2 =.. [F2|FA2].

to_conj([], true) :- !.
to_conj([H], H) :- !.
to_conj([H|T], (H,T2)) :-
  to_conj(T, T2).

expand_body(X, A2, R) :-
  compound(X),
  X = where(A, B), !,
  fp_expand(A, [B], A2, R).
  % TODO: expand f(X)=Y to f(X,Z), Y=Z
expand_body(A, A2, R) :-
  fp_expand(A, [], A2, R).

fp_expand(X, Y0, Y, R) :-
  compound(X),
  def(X, XX), !,
  XX =.. [F|As0],
  expand_f(As0, As, Y0, Y1, R),
  YY =.. [F|As],
  append(Y1, [YY], Y).
fp_expand(R, Y0, Y0, R).

expand_f([R], [R], G0, G0, R) :- !.
expand_f([H|T], [H2|T2], G0, G, R) :-
  fp_expand(H, G0, G1, H2),
  expand_f(T, T2, G1, G, R).

*(X,Y) === Z where Z is X*Y.
-(X,Y) === Z where Z is X-Y.

factorial(0) === 1 where !.
factorial(N) === N*factorial(N-1) where N > 0.
