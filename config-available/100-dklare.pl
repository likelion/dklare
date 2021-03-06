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

:- module('100-dklare', []).

/** <module> dklare
*/

:- multifile prolog:message_prefix_hook/2.

prolog:message_prefix_hook(thread, Prefix) :-
  get_time(Now),
  format_time(atom(Prefix0), '[%H:%M:%S.%f]', Now, posix),
  thread_self(Thread),
  format(atom(Prefix), '~w [~w]', [Prefix0,Thread]).

:- use_module(library(semweb/rdf_db), [rdf_register_prefix/2]).
:- rdf_register_prefix(d, 'http://dklare.org/2021/10/dklare#').
:- rdf_register_prefix(v, 'http://dklare.org/2021/10/variables#').

:- use_module(library(rdfs11)).
:- use_module(library(lambda)).
:- use_module(library(prelude)).
:- use_module(library(kb)).
:- use_module(library(resources)).

:- cp_after_load(
     ( bootstrap,
       broadcast(dklare(loading)),
       load_knowledge,
       broadcast(dklare(loaded))
     )
   ).

:- setting(dklare:knowledge_path, atom, 'knowledge/', 'Path to initial knowledge files').
:- setting(dklare:excluded_classes, list, [rdfs:'Resource',rdf:'List'], 'Classes excluded from RDFS inference').
:- setting(dklare:excluded_properties, list, [rdfs:seeAlso], 'Properties excluded from RDFS inference').
