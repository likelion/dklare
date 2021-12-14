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
                    op(1100, xfx, once),
                    op(600, yfx, @),
                    op(1100, fx, dklare_fun),
                    '@'/3,
                    eval/1,
                    eval/2 ] ).

:- multifile fun/1, '@'/3.
:- discontiguous fun/1, '@'/3.
:- dynamic fun/1, '@'/3.

eval(G) :-
  eval(G, R),
  print_term(R, []).

eval(G, R) :-
  prolog_load_context(module, Mo),
  phrase(expand_expression(G, true, T, R), L),
  Mo:maplist(assertz, L, Ref),
  call_cleanup(
    Mo:T,
    maplist(erase, Ref)
  ).

user:term_expansion((:-dklare_using(Import)), Clauses) :-
  prolog_load_context(file, File),
  file_name_extension(Path, 'pl', File),
  file_base_name(Path, M),
  prolog_load_context(stream, In),
  stream_property(In, reposition(true)),
  !,
  setup_call_cleanup(
    stream_property(In, position(Pos)),
    findall(Head, read_function(In, M, Head), Heads0),
    set_stream_position(In, Pos)
  ),
  sort(Heads0, Heads),
  ( is_list(Import)
  -> maplist(reexport, Import, Reexport),
     append(Reexport, Heads, Clauses)
  ; reexport(Import, Reexport),
    Clauses = [Reexport|Heads]
  ).

reexport(Import, (:-reexport(Import))) :-
  use_module(Import).

user:term_expansion((:-dklare_fun(Conj)), Clauses) :-
  prolog_load_context(file, File),
  file_name_extension(Path, 'pl', File),
  file_base_name(Path, M),
  dklare_fun(M, Conj, Clauses).

dklare_fun(M, (A,B), FAB) :- !,
  dklare_fun(M, A, FA),
  append(FA,FB,FAB),
  dklare_fun(M, B, FB).
dklare_fun(_, M:F, PA) :- !,
  dklare_fun_(M, F, PA).
dklare_fun(M, F, PA) :- !,
  dklare_fun_(M, F, PA).

dklare_fun_(M, F/A, [lambda:fun(M:F/A)|PA]) :-
  expand_partial_applications(M:F/A, PA),
  succ(A, A2),
  export(M:F/A2).

read_function(In, M, Head) :-
  repeat,
    read_term(In, Term, [syntax_errors(quiet),module(lambda)]),
    ( Term == end_of_file
    -> !, fail
    ; Term = (H === _),
      functor(H, F, A),
      Head = lambda:fun(M:F/A)
    ; fail
    ).

user:term_expansion(H === B, Clauses) :-
  strip_module(H, M, P),
  ( M == lambda
  -> prolog_load_context(module, Mo)
  ; Mo = M
  ),
  P =.. [F|A0],
  length(A0, Ar0),
  append(A0, [R], A1),
  succ(Ar0, Ar1),
  H2 =.. [F|A1],
  phrase(expand_body(B, B2, R), L),
  ( expand_partial_applications(Mo:F/Ar0, PA)
  -> append(PA, [H2:-B2|L], Clauses),
     export(Mo:F/Ar1)
  ; Clauses = [H2:-B2|L]
  ).

expand_partial_applications(Mo:F/Ar0, PA) :-
  Ar is Ar0 - 1,
  length(A, Ar),
  XX =.. [F|A],
  Head = lambda:'@'(XX,X,Y),
  \+ clause(Head, _),
  append(A, [X,Y], A2),
  Body =.. [F|A2],
  append(PA0, [Head:-Body], PA),
  expand_partial_applications_(Mo:F/Ar, PA0).

expand_partial_applications_(_:_/0, []) :- !.
expand_partial_applications_(Mo:F/Ar0, PA) :-
  Ar is Ar0 - 1,
  length(A, Ar),
  XX =.. [F|A],
  append(A, [X], A2),
  XXX =.. [F|A2],
  append(PA0, [lambda:'@'(XX,X,XXX)], PA),
  expand_partial_applications_(Mo:F/Ar, PA0).

expand_body(B, !, B) -->
  { var(B) }, !.
expand_body(X once Y, B, R) --> !,
  expand_condition(Y, Y1),
  { conj(Y1, !, Y2) },
  expand_expression(X, Y2, B, R).
expand_body(X where Y, B, R) --> !,
  expand_condition(Y, Y1),
  expand_expression(X, Y1, B, R).
expand_body(B0, B, R) -->
  expand_expression(B0, !, B, R).

conj(A, B, A) :-
  nonvar(B),
  B == true, !.
conj(A, B, B) :-
  nonvar(A),
  A == true, !.
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
       expand_partial_applications(Mo:RF/Ar, PA)
     },
     PA
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
  term_variables(Bo0, Bo1),
  without(F0, Bo1, Fr),
  reverse(Bo0, Bo).
decompose_lambda([H|T], Ta, L0, B, F) :-
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
