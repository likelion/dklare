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

:- module(rdfs11, [rdfs/3]).

:- use_module(library(semweb/rdf11)).
:- use_module(library(increval)).

:- table rdfs/3 as monotonic.
:- dynamic rdfq/3 as monotonic.

:- rdf_meta rdfs(r,r,o),
            rdfq(r,r,o).

:- listen(
     dklare(loaded),
     ( rdf_monitor(rdf_notify, [-all, +assert, +retract]),
       rdfs_warm
     )
   ).

rdf_notify(assert(S, P, O, _)) :-
  incr_propagate_calls(rdfq(S, P, O)).

rdf_notify(retract(S, P, O, _)) :-
  incr_invalidate_calls(rdfq(S, P, O)).

rdfs(P, rdf:type, rdf:'Property') :-                      % rdfs1
  rdfq(_, P, _).
rdfs(X, rdf:type, C) :-                                   % rdfs2
  rdfq(P, rdfs:domain, C),
  rdfq(X, P, _).
rdfs(Y, rdf:type, C) :-                                   % rdfs3
  rdfq(P, rdfs:range, C),
  rdfq(_, P, Y).
%rdfs(X, rdf:type, rdfs:'Resource') :-                     % rdfs4a
%  rdfq(X, _, _).
%rdfs(Y, rdf:type, rdfs:'Resource') :-                     % rdfs4b
%  rdfq(_, _, Y).
rdfs(P, rdfs:subPropertyOf, R) :-                         % rdfs5
  rdfq(P, rdfs:subPropertyOf, Q),
  rdfq(Q, rdfs:subPropertyOf, R).
%rdfs(P, rdfs:subPropertyOf, P) :-                         % rdfs6
%  rdfq(P, rdf:type, rdf:'Property').
rdfs(X, Q, Y) :-                                          % rdfs7
  rdfq(P, rdfs:subPropertyOf, Q),
  rdfq(X, P, Y).
%rdfs(C, rdfs:subClassOf, rdfs:'Resource') :-              % rdfs8
%  rdfq(C, rdf:type, rdfs:'Class').
rdfs(X, rdf:type, D) :-                                   % rdfs9
  rdfq(C, rdfs:subClassOf, D),
  rdfq(X, rdf:type, C).
%rdfs(C, rdfs:subClassOf, C) :-                            % rdfs10
%   rdfq(C, rdf:type, rdfs:'Class').
rdfs(C, rdfs:subClassOf, E) :-                            % rdfs11
  rdfq(C, rdfs:subClassOf, D),
  rdfq(D, rdfs:subClassOf, E).
 rdfs(P, rdfs:subPropertyOf, rdfs:member) :-              % rdfs12
   rdfq(P, rdf:type, rdfs:'ContainerMembershipProperty').
 rdfs(X, rdfs:subClassOf, rdfs:'Literal') :-              % rdfs13
   rdfq(X, rdf:type, rdfs:'Datatype').

rdfq(S, P, O) :-
  rdfs(S, P, O).
rdfq(S, P, O) :-
  rdf(S, P, O).

rdfs_warm :-
  print_message(information, warming),
  get_time(T0),
  statistics(cputime, CPU0),
  ignore((
    rdfs(_, rdf:type, _),
    rdfs(_, rdfs:subClassOf, _),
    rdfs(_, rdfs:subPropertyOf, _)
  )),
  statistics(cputime, CPU1),
  get_time(T1),
  Wall is T1-T0,
  CPU is CPU1-CPU0,
  aggregate_all(sum(S), (
    current_table(rdfs11:A, T),
    A =.. [rdfs,_,_,_],
    trie_property(T, value_count(S))
  ), Triples),
  aggregate_all(sum(S), (
    current_table(rdfs11:A, T),
    A =.. [rdfs,_,_,_],
    trie_property(T, size(S))
  ), Bytes),
  print_message(information, inferred(Triples, Bytes)),
  print_message(information, warmed(Wall, CPU)).

:- multifile prolog:message/3.

prolog:message(warming) -->
  [ 'Warming RDFS...' ].

prolog:message(inferred(Triples, Bytes)) -->
  [ 'Inferred ~D triples (~D bytes)'-[Triples, Bytes] ].

prolog:message(warmed(Wall, CPU)) -->
  [ 'Warmed RDFS in ~3f sec. (~3f sec. on CPU)'-[Wall, CPU] ].

% -- DEBUG

rdfst :-
  forall((
      current_table(rdfs11:A, T),
      trie_property(T, size(S))
    ), (
      once(trie_gen(T, _, _)),
      debug(_, '~w: ~w', [A,S]),
      forall(trie_gen(T, K, V), debug(_, '  ~w => ~w', [K,V]))
    ; true
    )
  ).

rdfstt :-
  forall((
      current_table(rdfs11:A, T),
      trie_property(T, size(S))
    ), (
      once(trie_gen(T, _, _)),
      debug(_, '~w: ~w', [A,S]),
      aggregate_all(count, (
        trie_gen(T, _, _)
      ), Countr),
      debug(_, '  ~w', [Countr])
    ; true
    )
  ).

rdfss :-
  aggregate_all(sum(S), (
    current_table(rdfs11:A, T),
    A =.. [rdfs,_,_,_],
    trie_property(T, value_count(S))
  ), Countr),
  aggregate_all(sum(S), (
    current_table(rdfs11:A, T),
    A =.. [rdfs,_,_,_],
    trie_property(T, size(S)),
    debug(_, 'table ~w: ~D', [A,S])
  ), RDFR),
  debug(_, 'RDFS: ~D inferred triples (~D bytes)', [Countr,RDFR]).
