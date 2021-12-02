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
                    rdfs_only/3,
                    rdfs_estimate_complexity/4
                  ]).

:- use_module(library(semweb/rdf11)).
:- use_module(library(increval)).
:- use_module(library(utils)).

:- table rdfs_/3 as monotonic.
:- dynamic rdfq/3 as monotonic.

:- thread_local class/1, property/1, intransaction/0, inferred/1.

:- rdf_meta class(r),
            property(r),
            rdfs(r,r,o),
            rdfs_only(r,r,o),
            rdfi(r,r,o),
            rdfq(r,r,o),
            rdfs_estimate_complexity(r,r,o,-).

:- listen(dklare(loaded),
     ( create_thread(rdfs, (set_exclusions, handle_messages)),
       rdf_monitor(rdf_notify, [-all, +assert, +retract, +transaction, +load]),
       listen(settings(changed(dklare:excluded_classes, _, _)),
         async_message(rdfs, set_exclusions)
       ),
       listen(settings(changed(dklare:excluded_properties, _, _)),
         async_message(rdfs, set_exclusions)
       ),
       prolog_listen(rdfs_/3, rdfs_notify),
       sync_message(rdfs, true)
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
  async_message(rdfs, incr_propagate_calls(rdfq(S, P, O2))).

rdf_notify(retract(S, P, O, _)) :-
  from_rdf_db(O, O2),
  async_message(rdfs, incr_invalidate_calls(rdfq(S, P, O2))).

rdf_notify(load(begin(0), _)) :-
  async_message(rdfs, collect).

rdf_notify(load(end(0), _)) :-
  async_message(rdfs, reinfer).

rdf_notify(transaction(begin(0), _)) :-
  async_message(rdfs, collect).

rdf_notify(transaction(end(0), _)) :-
  async_message(rdfs, reinfer).

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

rdfs_notify(new_answer, rdfs11:T) :-
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
  ( \+ \+ rdf(S, P, O)
  -> ( rdfs_only(S, P, O)
     ; rdf(S, P, O)
     )
  ; rdfs_only(S, P, O)
  ).

% TODO: do inferencing of rdfs:Resource, rdf:Property on the fly here

rdfs_only(S, P, O) :-
  sync_message(rdfs, rdfs11:rdfs_(S, P, O), Result),
  member(rdfs11:rdfs_(S, P, O), Result).

rdfs_(S, P, O) :-
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
  rdfs_(S, P, O).
rdfq(S, P, O) :-
  rdf(S, P, O).

rdfs_reinit :-
  print_message(information, warming),
  abolish_table_subgoals(rdfs11:rdfs_(_,_,_)),
  time(ignore(rdfs_(_, _, _))),
  print_rdfs_totals(none).

rdfs_estimate_complexity(S, P, O, C) :-
  rdf_estimate_complexity(S, P, O, C0),
  sync_message(rdfs, (
    current_table(rdfs11:rdfs_(S, P, O), T),
    trie_property(T, value_count(C1))
  )),
  ( integer(C1)
  -> plus(C0, C1, C)
  ; C = C0
  ).

print_rdfs_totals(Mode) :-
  Ts = count(0),
  Vs = count(0),
  Bs = count(0),
  forall((
    current_table(rdfs11:A, T),
    trie_property(T, size(B)),
    trie_property(T, value_count(C)),
    C > 0,
    ( A =@= rdfs_(_,_,_)
    -> nb_setarg(1, Ts, C)
    ; true
    )
  ), (
    arg(1, Vs, V0),
    plus(V0, C, V1),
    nb_setarg(1, Vs, V1),
    arg(1, Bs, B0),
    plus(B0, B, B1),
    nb_setarg(1, Bs, B1),
    ( Mode == none
    -> true
    ; ( V0 == 0,
        C > 0
      -> print_message(information, table_header),
         print_message(information, table_divider)
      ; true
      ),
      print_message(information, table_totals(A, C, B)),
      ( Mode == tbl
      -> forall(
           trie_gen(T, K),
           print_message(information, table_variant(K))
         )
      ; true
      )
    )
  )),
  arg(1, Ts, Triples),
  arg(1, Vs, Variants),
  arg(1, Bs, Bytes),
  ( Mode \== none,
    Triples > 0
  -> print_message(information, table_divider)
  ; true
  ),
  print_message(information, inferred(Triples, Variants, Bytes)),
  print_message(information, total_space_used).

:- multifile prolog:message/3.

prolog:message(warming) -->
  [ 'Warming RDFS...' ].

prolog:message(table_header) -->
  [ '~` t~w~15+ ~` t~w~15+  ~w'-['Variants','Bytes','Variant'] ].

prolog:message(table_divider) -->
  [ '------------------------------------------------------' ].

prolog:message(table_totals(Table, Variants, Bytes)) -->
  [ '~` t~D~15+ ~` t~D~15+  ~@'-[Variants, Bytes, utils:debug_args(Table)] ].

prolog:message(table_variant(Variant)) -->
  [ '~` t~34+~@'-[utils:debug_args(Variant)] ].

prolog:message(inferred(Triples, Variants, Bytes)) -->
  [ 'Inferred ~D triples (~D variants) [~D bytes]'-[Triples, Variants, Bytes] ].

prolog:message(total_space_used) -->
  { statistics(table_space_used, TSU) },
  [ 'Table space used: ~D bytes'-[TSU] ].

% -- DEBUG

rdfst :-
  sync_message(rdfs, print_rdfs_totals(tbl)).

rdfsc :-
  sync_message(rdfs, print_rdfs_totals(cnt)).
