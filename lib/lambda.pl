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

def(X, XX, R) :-
  compound(X),
  X =.. [F|A],
  atom_concat('_', F, FF),
  append(A, [R], AA),
  XX =.. [FF|AA].

term_expansion(H === B, H2 :- B2) :-
  def(H, H2, R),
  expand_body(B, BE, R),
  to_conj(BE,B2).

to_conj([], true) :- !.
to_conj([H], H) :- !.
to_conj([H|T], (H,T2)) :-
  to_conj(T, T2).

expand_body(X, A2, R) :-
  compound(X),
  X = where(A, B), !,
  fp_expand(A, [B], A2, R).
expand_body(A, A2, R) :-
  fp_expand(A, [], A2, R).

fp_expand(X, Y0, Y, R) :-
  compound(X),
  X = A@B, !,
  expand_f([A,B,_], As, Y0, Y1, R),
  YY =.. ['_@'|As],
  append(Y1, [YY], Y).
fp_expand(X, Y0, Y, [RA|RB]) :-
  compound(X),
  X = [A|B], !,
  fp_expand(A, Y0, Y1, RA),
  fp_expand(B, Y1, Y, RB).
fp_expand(X, Y0, Y, R) :-
  def(X, XX, _),
  XX =.. [F|As0],
  same_length(As0, AA),
  UX =.. [F|AA],
  prolog_load_context(module, M),
  clause(M:UX, _), !,
  expand_f(As0, As, Y0, Y1, R),
  YY =.. [F|As],
  append(Y1, [YY], Y).
fp_expand(R, Y0, Y0, R).

expand_f([R], [R], G0, G0, R) :- !.
expand_f([H|T], [H2|T2], G0, G, R) :-
  fp_expand(H, G0, G1, H2),
  expand_f(T, T2, G1, G, R).
