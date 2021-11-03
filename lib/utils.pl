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
                   create_thread/2,
                   async_message/2,
                   sync_message/2,
                   sync_message/3,
                   handle_messages/0
                 ]).

:- use_module(library(semweb/rdf11)).

:- meta_predicate create_thread(+, :),
                  async_message(+, :),
                  sync_message(+, :),
                  sync_message(+, :, -).

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

async_message(Thread, Goal) :-
  thread_send_message(Thread, Goal).

sync_message(Thread, Goal) :-
  sync_message_(Thread, one, Goal, Goal).
sync_message(Thread, Goal, Result) :-
  sync_message_(Thread, all, Goal, Result).

sync_message_(Thread, Quantifier, Goal, Result) :-
  gensym('__queue', Alias),
  call_cleanup(
    ( message_queue_create(Queue, [alias(Alias)]),
      Message =.. [Quantifier,Goal,Queue],
      thread_send_message(Thread, Message),
      thread_get_message(Queue, Result)
    ),
    message_queue_destroy(Queue)
  ).

handle_messages :-
  catch(
    ( repeat,
        thread_get_message(Message),
        handle_message(Message),
        fail
    ),
    E,
    ( E \== '$aborted'
    -> print_message(error, E)
    ; true
    )
  ).

handle_message(one(Goal, Queue)) :- !,
  call_cleanup(
    once(Goal),
    thread_send_message(Queue, Goal)
  ).
handle_message(all(Goal, Queue)) :- !,
  call_cleanup(
    findall(Goal, Goal, Result),
    thread_send_message(Queue, Result)
  ).
handle_message(Goal) :-
  once(Goal).
