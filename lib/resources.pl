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

:- rdf_meta resource(t),
            read_function(r, -, -, -),
            read_expression(r, -),
            read_application(r, -),
            read_args(r, -),
            variable(r, -, r),
            literal(o, -, -).

resource(Triples) :-
  triplesort(Triples, SortedTriples),
  match(SortedTriples).

triplesort([], []) :- !.
triplesort(Triples, SortedTriples) :-
  maplist(check_type, Triples),
  maplist(estimate, Triples, EstimatedTriples),
  keysort(EstimatedTriples, SortedTriples).

check_type(_-rdfs(S,P,_)) :- !,
  check_type(rdf(S,P,_,_)).
check_type(rdfs(S,P,_)) :- !,
  check_type(rdf(S,P,_,_)).
check_type(_-rdf(S,P,_,G)) :- !,
  check_type(rdf(S,P,_,G)).
check_type(rdf(S,P,_,G)) :-
  ( var(S) ; atom(S) ), !,
  ( var(P) ; ( atom(P), \+ rdf_is_bnode(P) ) ), !,
  ( var(G) ; atom(G) ), !.

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

read_function(IRI, Functor, Arity, Functions) :-
  resource([ rdfs(IRI,rdf:type,d:'Function'),
             rdfs(IRI,d:define,Lambda) ]),
  read_expression(IRI, Functor),
  ( rdf_list(Lambda)
  -> rdf_list(Lambda, Lambdas),
     maplist(read_lambda(Functor, Arity), Lambdas, Functions)
  ; read_lambda(Functor, Arity, Lambda, Function),
    Functions = [Function]
  ).

read_lambda(Functor, Arity, IRI, Lambda) :-
  resource([rdfs(IRI,d:arg,Args)]),
  read_args(Args, A),
  Head =.. [Functor|A],
  length(A, Arity),
  resource([rdfs(IRI,d:exp,Exp)]),
  read_expression(Exp, E),
  ( resource([rdfs(IRI,d:con,Con)]),
    read_expression(Con, C)
  -> Lambda0 = '==='(Head,'where'(E,C))
  ; Lambda0 = '==='(Head,E)
  ),
  varnumbers_names(Lambda0, Lambda, _).

read_expression(IRI, Expression) :-
  ( atom(IRI)
  -> ( variable(IRI, Expression, v:'')
     -> true
     ; ( resource([ rdfs(IRI,rdf:type,d:'Function'),
                    rdfs(IRI,d:functor,^^(FS,_)) ])
       -> atom_string(Expression, FS)
       ; read_application(IRI, Expression)
       -> true
       ; rdf_is_bnode(IRI)
       -> phrase(read_pattern(IRI, _), Expression)
       ; Expression = IRI
       )
     )
  ; ( literal(IRI, FS, string)
    -> ( FS == "[]"
       -> Expression = []
       ; atom_string(Expression, FS)
       )
    ; literal(IRI, Expression, number)
    )
  ).

variable(IRI, '$VAR'(V), Prefix) :-
  atom_concat(Prefix, V, IRI).

literal(^^(S, xsd:string), S, string) :- !.
literal(@(S, _), S, string) :- !.
literal(^^(N, _), N, number) :-
  number(N).

read_application(rdf:nil, []) :- !.
read_application(IRI, Application) :-
  resource([rdf(IRI,rdf:first,First,_),
            rdf(IRI,rdf:rest,Rest,_)]),
  read_args(Rest, Args),
  read_expression(First, Functor),
  ( compound(Functor)
  -> Application =.. ['@',Functor|Args]
  ; Application =.. [Functor|Args]
  ).

read_args(rdf:nil, []) :- !.
read_args(IRI, [H|T]) :-
  resource([rdf(IRI,rdf:first,First,_),
            rdf(IRI,rdf:rest,Rest,_)]),
  read_expression(First, H),
  read_args(Rest, T).

read_pattern(IRI, Id) -->
  { findall(P-O, resource([rdf(IRI,P,O,_)]), POs) },
  read_triples(IRI, Id, POs).

read_triples(_, _, []) --> !.

read_triples(IRI, Id, [P-O|T]) -->
  { rdf_is_bnode(O), !,
    ( variable(P, PV, v:'')
    -> true
    ; PV = P
    )
  },
  [t(Id, PV, OId)],
  read_pattern(O, OId),
  read_triples(IRI, Id, T).

read_triples(IRI, Id, [P-O|T]) -->
  { rdf_global_id(d:id, P), !,
    ( variable(O, Id, v:'')
     -> true
    ; Id = O
    )
  },
  read_triples(IRI, Id, T).

read_triples(IRI, Id, [P-O|T]) -->
  { ( variable(P, PV, v:'')
    -> true
    ; PV = P
    ),
    ( variable(O, OV, v:'')
    -> true
    ; OV = O
    )
  },
  [t(Id, PV, OV)],
  read_triples(IRI, Id, T).
