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

:- table rdfs_only/3 as monotonic.
:- dynamic rdfq/3 as monotonic.

:- rdf_meta rdfs(r,r,o),
            rdfs_only(r,r,o),
            rdfi(r,r,o),
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

rdfs(S, P, O) :-
  rdf(S, P, O).
rdfs(S, P, O) :-
  rdfs_only(S, P, O).

rdfs_only(S, P, O) :-
  rdfi(S, P, O),
  \+ rdf(S, P, O).

rdfi(P, rdf:type, rdf:'Property') :-                      % rdfs1
  rdfq(_, P, _).
rdfi(X, rdf:type, C) :-                                   % rdfs2
  rdfq(P, rdfs:domain, C),
  rdfq(X, P, _).
rdfi(Y, rdf:type, C) :-                                   % rdfs3
  rdfq(P, rdfs:range, C),
  rdfq(_, P, Y),
  atom(Y).
rdfi(X, rdf:type, rdfs:'Resource') :-                     % rdfs4a
  rdfq(X, _, _).
rdfi(Y, rdf:type, rdfs:'Resource') :-                     % rdfs4b
  rdfq(_, _, Y),
  atom(Y).
rdfi(P, rdfs:subPropertyOf, R) :-                         % rdfs5
  rdfq(P, rdfs:subPropertyOf, Q),
  rdfq(Q, rdfs:subPropertyOf, R).
%rdfi(P, rdfs:subPropertyOf, P) :-                         % rdfs6
%  rdfq(P, rdf:type, rdf:'Property').
rdfi(X, Q, Y) :-                                          % rdfs7
  rdfq(P, rdfs:subPropertyOf, Q),
  rdfq(X, P, Y).
rdfi(C, rdfs:subClassOf, rdfs:'Resource') :-              % rdfs8
  rdfq(C, rdf:type, rdfs:'Class').
rdfi(X, rdf:type, D) :-                                   % rdfs9
  rdfq(C, rdfs:subClassOf, D),
  rdfq(X, rdf:type, C).
%rdfi(C, rdfs:subClassOf, C) :-                            % rdfs10
%   rdfq(C, rdf:type, rdfs:'Class').
rdfi(C, rdfs:subClassOf, E) :-                            % rdfs11
  rdfq(C, rdfs:subClassOf, D),
  rdfq(D, rdfs:subClassOf, E).
rdfi(P, rdfs:subPropertyOf, rdfs:member) :-               % rdfs12
  rdfq(P, rdf:type, rdfs:'ContainerMembershipProperty').
rdfi(X, rdfs:subClassOf, rdfs:'Literal') :-               % rdfs13
  rdfq(X, rdf:type, rdfs:'Datatype').

rdfq(S, P, O) :-
  rdfs_only(S, P, O).
rdfq(S, P, O) :-
  rdf(S, P, O).

rdfs_warm :-
  print_message(information, warming),
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
      shorten(A, A2),
      debug(_, '~w: ~w triples (~D bytes)', [A2,C,S]),
      forall(
        trie_gen(T, K),
        ( shorten(K, K2),
          debug(_, '  ~w', [K2])
        )
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
    shorten(A, A2),
    debug(_, '~w:', [A2]),
    debug(_, '  ~w triples (~D bytes)', [C,S])
  )).

shorten(T, T2) :-
  T =.. [F|A],
  maplist(shorten_iri, A, A2),
  T2 =.. [F|A2].

shorten_iri(V, V) :-
  var(V), !.
shorten_iri(S^^_, S) :- !.
shorten_iri(S@_, S) :- !.
shorten_iri(I, I2) :-
  atom(I), !,
  rdf_global_id(I2, I).
shorten_iri(I, I).
