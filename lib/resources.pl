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

:- module(resources, [resource/1]).

:- use_module(library(semweb/rdf11)).
:- use_module(library(rdfs11)).

:- rdf_meta resource(t).

resource(Triples) :-
  triplesort(Triples, SortedTriples),
  match(SortedTriples).

triplesort([], []) :- !.
triplesort(Triples, SortedTriples) :-
  maplist(estimate, Triples, EstimatedTriples),
  keysort(EstimatedTriples, SortedTriples).

estimate(rdf(S,P,O,G), C-rdf(S,P,O,G)) :- !,
  rdf_estimate_complexity(S, P, O, C).
estimate(_-rdf(S,P,O,G), C-rdf(S,P,O,G)) :- !,
  rdf_estimate_complexity(S, P, O, C).
estimate(rdfs(S,P,O), C-rdfs(S,P,O)) :- !,
  rdfs_estimate_complexity(S, P, O, C).
estimate(_-rdfs(S,P,O), C-rdfs(S,P,O)) :-
  rdfs_estimate_complexity(S, P, O, C).

match([]) :- !.
match([_-Triple]) :- !,
  Triple.
match([_-Triple|T]) :- !,
  Triple,
  triplesort(T, SortedT),
  match(SortedT).
