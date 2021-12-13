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

:- module(kb, [bootstrap/0,
               load_knowledge/0]).

:- use_module(library(semweb/rdf11)).
:- use_module(library(semweb/turtle)).
:- use_module(library(settings)).
:- use_module(library(utils)).

bootstrap :-
  catch(
    rdf_load('dklare.ttl', [graph(dklare),register_namespaces(true)]),
    _,
    true
  ).

load_knowledge :-
  setting(dklare:knowledge_path, Prefix0),
  ensure_slash(Prefix0, Prefix),
  atom_concat(Prefix, 'Manifest.ttl', Manifest),
  catch((
    uuid(UUID),
    atom_concat('$', UUID, ManifestGraph),
    rdf_load(Manifest, [graph(ManifestGraph),register_namespaces(true)]),
    call_cleanup(
      findall(Path,
        ( rdf(File, rdf:type, d:'KnowledgeFile', ManifestGraph),
          ignore(rdf(File, d:graph, ^^(GraphS, xsd:string), ManifestGraph)),
          atom_concat(Prefix, File, Path),
          atom_string(Graph, GraphS),
          atom_concat('_:', File, AnonPrefix),
          catch(
            rdf_load(Path, [graph(Graph),multifile(true),anon_prefix(AnonPrefix)]),
            _,
            true
          )
        ),
        Paths
      ),
      rdf_unload_graph(ManifestGraph)
    )
  ), _, true),
  ( var(Paths)
  -> Paths = []
  ; true
  ),
  catch((
    atom_concat(Prefix, '*', Search),
    expand_file_name(Search, Files),
    subtract(Files, [Manifest|Paths], Rest),
    forall(
      ( member(Path, Rest),
        exists_file(Path)
      ),
      rdf_load(Path)
    )
  ), _, true).
