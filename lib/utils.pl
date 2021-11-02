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

:- module(utils, [ debug_args/1,
                   from_rdf_db/2,
                   create_thread/2
                  ]).

:- use_module(library(semweb/rdf11)).

:- meta_predicate create_thread(+, :).

:- rdf_meta shorten(o, -),
            from_rdf_db(t, o).

debug_args(T) :-
  T =.. [_|A],
  maplist(shorten, A, A2),
  maplist(debug_format, A2, AF),
  atomic_list_concat(AF, ', ', CF),
  atomic_list_concat(['(', CF, ')'], F),
  format(current_output, F, A2).

debug_format(V, '~w') :-
  var(V), !.
debug_format(_:_, '~w') :- !.
debug_format(_, '~q').

shorten(V, V) :-
  var(V), !.
shorten(S^^xsd:string, S) :- !.
shorten(N^^xsd:integer, N) :- !.
shorten(N^^xsd:decimal, N) :- !.
shorten(N^^xsd:double, N) :- !.
shorten(I, PN) :-
  atom(I), !,
  rdf_global_id(PN, I).
shorten(I, I).

from_rdf_db(literal(type(xsd:string, A)), S^^xsd:string) :- !,
  atom_string(A, S).
from_rdf_db(literal(type(xsd:integer, A)), N^^xsd:integer) :- !,
  atom_number(A, N).
from_rdf_db(literal(type(xsd:decimal, A)), N^^xsd:decimal) :- !,
  atom_number(A, N).
from_rdf_db(literal(type(xsd:double, A)), N^^xsd:double) :- !,
  atom_number(A, N).
from_rdf_db(literal(type(T, A)), S^^T) :- !,
  atom_string(A, S).
from_rdf_db(literal(lang(L, A)), S@L) :- !,
  atom_string(A, S).
from_rdf_db(literal(A), S^^xsd:string) :- !,
  atom_string(A, S).
from_rdf_db(R, R).

create_thread(Alias, Goal) :-
  ( thread_property(Id, alias(Alias)),
    is_thread(Id)
  -> ( thread_property(Id, status(running))
     -> thread_signal(Id, throw(stop))
     ; true
     ),
     thread_join(Id)
  ; true
  ),
  thread_create(Goal, _, [alias(Alias)]).
