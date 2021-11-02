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

:- module(rdfs11, [ rdfs/3,
                    rdfs_only/3
                  ]).

:- use_module(library(semweb/rdf11)).
:- use_module(library(increval)).
:- use_module(library(utils)).

:- table rdfs_only/3 as monotonic.
:- dynamic rdfq/3 as monotonic.

:- thread_local class/1, property/1, intransaction/0, inferred/1.

:- rdf_meta class(r),
            property(r),
            rdfs(r,r,o),
            rdfs_only(r,r,o),
            rdfi(r,r,o),
            rdfq(r,r,o).

:- listen(dklare(loaded),
     ( rdf_monitor(rdf_notify, [-all, +assert, +retract, +transaction, +load]),
       listen(settings(changed(dklare:excluded_classes, _, _)),
         thread_signal(main, set_exclusions)
       ),
       listen(settings(changed(dklare:excluded_properties, _, _)),
         thread_signal(main, set_exclusions)
       ),
       prolog_listen(rdfs_only/3, rdfs_new),
       set_exclusions
     )
   ).

set_exclusions :-
  setting(dklare:excluded_classes, Classes),
  setting(dklare:excluded_properties, Properties),
  retractall(class(_)),
  retractall(property(_)),
  maplist(exclude(class), Classes),
  maplist(exclude(property), Properties),
  rdfs_reinit.

exclude(Term, Resource0) :-
  rdf_global_id(Resource0, Resource),
  exclude_(Term, Resource).
exclude_(Term, Resource) :-
  call(Term, Resource), !.
exclude_(Term, Resource) :-
  T =.. [Term,Resource],
  assertz(T).

rdf_notify(assert(S, P, O, _)) :-
  from_rdf_db(O, O2),
  thread_signal(main, incr_propagate_calls(rdfq(S, P, O2))).

rdf_notify(retract(S, P, O, _)) :-
  from_rdf_db(O, O2),
  thread_signal(main, incr_invalidate_calls(rdfq(S, P, O2))).

rdf_notify(load(begin(0), _)) :-
  thread_signal(main, collect).

rdf_notify(load(end(0), _)) :-
  thread_signal(main, reinfer).

rdf_notify(transaction(begin(0), _)) :-
  thread_signal(main, collect).

rdf_notify(transaction(end(0), _)) :-
  thread_signal(main, reinfer).

collect :-
  ( intransaction
  -> true
  ; assertz(intransaction)
  ).

reinfer :-
  retractall(intransaction),
  incr_table_update,
  repeat,
    ( retract(inferred(T))
    -> notify_inferred(T),
       fail
    ; true
    ).

rdfs_new(new_answer, rdfs11:T) :-
  ( intransaction
  -> ( inferred(T)
     -> true
     ; assertz(inferred(T))
     )
  ; notify_inferred(T)
  ).

notify_inferred(T) :-
  debug(_, 'Inferred ~@', [debug_args(T)]).

rdfs(S, P, O) :-
  rdf(S, P, O).
rdfs(S, P, O) :-
  rdfs_only(S, P, O).

rdfs_only(S, P, O) :-
  rdfi(S, P, O),
  \+ rdf(S, P, O).

rdfi(P, rdf:type, rdf:'Property') :-                      % rdfs1
  \+ class(rdf:'Property'),
  rdfq(_, P, _).
rdfi(X, rdf:type, C) :-                                   % rdfs2
  rdfq(P, rdfs:domain, C),
  \+ class(C),
  rdfq(X, P, _).
rdfi(Y, rdf:type, C) :-                                   % rdfs3
  rdfq(P, rdfs:range, C),
  \+ class(C),
  rdfq(_, P, Y),
  atom(Y).
rdfi(X, rdf:type, rdfs:'Resource') :-                     % rdfs4a
  \+ class(rdfs:'Resource'),
  rdfq(X, _, _).
rdfi(Y, rdf:type, rdfs:'Resource') :-                     % rdfs4b
  \+ class(rdfs:'Resource'),
  rdfq(_, _, Y),
  atom(Y).
rdfi(P, rdfs:subPropertyOf, R) :-                         % rdfs5
  rdfq(P, rdfs:subPropertyOf, Q),
  rdfq(Q, rdfs:subPropertyOf, R),
  \+ property(R).
%rdfi(P, rdfs:subPropertyOf, P) :-                         % rdfs6
%  rdfq(P, rdf:type, rdf:'Property').
rdfi(X, Q, Y) :-                                          % rdfs7
  rdfq(P, rdfs:subPropertyOf, Q),
  \+ property(Q),
  rdfq(X, P, Y),
  ( rdf_equal(rdf:type, Q)
  -> \+ class(Y)
  ; true
  ).
rdfi(C, rdfs:subClassOf, rdfs:'Resource') :-              % rdfs8
  \+ class(rdfs:'Resource'),
  rdfq(C, rdf:type, rdfs:'Class').
rdfi(X, rdf:type, D) :-                                   % rdfs9
  rdfq(C, rdfs:subClassOf, D),
  \+ class(D),
  rdfq(X, rdf:type, C).
%rdfi(C, rdfs:subClassOf, C) :-                            % rdfs10
%   rdfq(C, rdf:type, rdfs:'Class').
rdfi(C, rdfs:subClassOf, E) :-                            % rdfs11
  rdfq(C, rdfs:subClassOf, D),
  rdfq(D, rdfs:subClassOf, E),
  \+ class(E).
rdfi(P, rdfs:subPropertyOf, rdfs:member) :-               % rdfs12
  \+ property(rdfs:member),
  rdfq(P, rdf:type, rdfs:'ContainerMembershipProperty').
rdfi(X, rdfs:subClassOf, rdfs:'Literal') :-               % rdfs13
  \+ class(rdfs:'Literal'),
  rdfq(X, rdf:type, rdfs:'Datatype').

rdfq(S, P, O) :-
  rdfs_only(S, P, O).
rdfq(S, P, O) :-
  rdf(S, P, O).

rdfs_reinit :-
  print_message(information, warming),
  abolish_table_subgoals(rdfs11:rdfs_only(_,_,_)),
  time(ignore(rdfs_only(_, _, _))),
  Ts = count(0),
  Vs = count(0),
  Bs = count(0),
  forall((
    current_table(rdfs11:A, T),
    trie_property(T, value_count(C)),
    C > 0,
    ( A =@= rdfs_only(_,_,_)
    -> nb_setarg(1, Ts, C)
    ; true
    ),
    trie_property(T, size(B))
  ),(
    arg(1, Vs, V0),
    plus(V0, C, V1),
    nb_setarg(1, Vs, V1),
    arg(1, Bs, B0),
    plus(B0, B, B1),
    nb_setarg(1, Bs, B1)
  )),
  arg(1, Ts, Triples),
  arg(1, Vs, Variants),
  arg(1, Bs, Bytes),
  print_message(information, inferred(Triples, Variants, Bytes)).

:- multifile prolog:message/3.

prolog:message(warming) -->
  [ 'Warming RDFS...' ].

prolog:message(inferred(Triples, Variants, Bytes)) -->
  [ 'Inferred ~D triples (~D variants) [~D bytes]'-[Triples, Variants, Bytes] ].

% -- DEBUG

rdfst :-
  forall((
      current_table(rdfs11:A, T),
      trie_property(T, size(S)),
      trie_property(T, value_count(C)),
      C > 0
    ), (
      debug(_, '~@: ~w variants (~D bytes)', [debug_args(A),C,S]),
      forall(
        trie_gen(T, K),
        debug(_, '  ~@', [debug_args(K)])
      )
    ; true
    )
  ).

rdfsc :-
  forall((
    current_table(rdfs11:A, T),
    trie_property(T, size(S)),
    trie_property(T, value_count(C)),
    C > 0
  ),(
    debug(_, '~@:', [debug_args(A)]),
    debug(_, '  ~w variants (~D bytes)', [C,S])
  )).
