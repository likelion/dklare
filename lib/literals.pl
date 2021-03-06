
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

:- module(literals, [object_to_term/2,
                     literal_to_term/2]).

:- use_module(library(semweb/rdf11)).

:- rdf_meta object_to_term(o, -),
            literal_to_term(o, -).

object_to_term(O, T) :-
  literal_to_term(O, T), !.
object_to_term(T, T).

literal_to_term(^^(true, xsd:boolean), true) :- !.
literal_to_term(^^(false, xsd:boolean), false) :- !.
literal_to_term(^^(S, xsd:string), S) :-
  string(S), !.
literal_to_term(^^(N, _), N) :-
  number(N), !.
literal_to_term(^^(L, T), ^^(L, T)) :- !.
literal_to_term(@(S, en), S) :-
  string(S), !.
literal_to_term(@(S, L), @(S, L)) :- !.
