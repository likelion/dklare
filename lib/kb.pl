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

:- module(kb, [load_knowledge/0]).

:- use_module(library(semweb/rdf11)).
:- use_module(library(semweb/turtle)).

load_knowledge :-
  catch((
    uuid(UUID),
    atom_concat('$manifest-', UUID, ManifestGraph),
    rdf_load('knowledge/Manifest.ttl', [graph(ManifestGraph)]),
    call_cleanup(
      forall(
        rdf(File, rdf:type, d:'KnowledgeFile', ManifestGraph),
        ( ignore(rdf(File, d:graph, ^^(GraphS, xsd:string), ManifestGraph)),
          atom_concat('knowledge/', File, Path),
          atom_string(Graph, GraphS),
          atom_concat('_:', File, AnonPrefix),
          rdf_load(Path, [graph(Graph),multifile(true),anon_prefix(AnonPrefix)])
        )
      ),
      rdf_unload_graph(ManifestGraph)
    )),
    _,
    true
  ).
