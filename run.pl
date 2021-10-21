#!/usr/bin/env swipl

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

user:file_search_path(library, lib).

:- use_module(library(semweb/rdf_persistency)).
:- use_module(library(http/http_server)).

:- initialization(call_cleanup(main, set_prolog_flag(verbose, silent))).

:- http_handler(root(.),
                http_redirect(moved, location_by_id(home_page)),
                []).
:- http_handler(root(home), home_page, []).

home_page(_) :-
  reply_html_page(
    title('dklare'),
    [ h2('dklare standalone') ]
  ).

:- meta_predicate user:cp_after_load(0).
:- dynamic after_load_goal/1.

user:cp_after_load(Goal) :-
  ( after_load_goal(Goal)
  -> true
  ; assert(after_load_goal(Goal))
  ).

main :-
  debug(pengine(debug)),
  print_message(banner, dklare),
  consult('settings.db'),
  rdf_attach_db('RDF-store', []),
  expand_file_name('config-available/*.pl', Configs),
  maplist(use_module, Configs),
  http_server([port(3020)]),
  ( after_load_goal(Goal)
  -> call(Goal),
     retractall(after_load_goal(_))
  ; true
  ).

:- multifile prolog:message/3.

prolog:message(dklare) -->
  ['        ____   __
   ____/ / /__/ /___ _________
  / __  / //_/ / __ `/ ___/ _ \\
 / /_/ / ,< / / /_/ / /  /  __/
 \\__,_/_/|_/_/\\__,_/_/   \\___/
' ] .
