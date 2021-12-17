
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

:- module(literals, [literal_term_type/3,
                     object_to_term/2]).

:- use_module(library(semweb/rdf11)).

:- rdf_meta literal_term_type(o, -, -),
            object_to_term(o, -).

literal_term_type(^^(S, xsd:string), A, string) :- !,
  to_atom(S, A).
literal_term_type(@(S, _), A, string) :- !,
  to_atom(S, A).
literal_term_type(^^(N, _), N, number) :-
  number(N).

to_atom(A, A) :-
  atom(A), !.
to_atom(S, A) :-
  string(S),
  atom_string(A, S).

object_to_term(^^(true, xsd:boolean), true) :- !.
object_to_term(^^(false, xsd:boolean), false) :- !.
object_to_term(^^(S, xsd:string), S) :-
  string(S), !.
object_to_term(^^(N, _), N) :-
  number(N), !.
object_to_term(^^(L, T), ^^(L, T)) :- !.
object_to_term(@(S, en), S) :-
  string(S), !.
object_to_term(@(S, L), @(S, L)) :- !.
object_to_term(A, A).
