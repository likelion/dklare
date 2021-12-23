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

:- module(resources, [match/1,
                      read_function/4,
                      read_expression_list/2,
                      read_expression/2,
                      read_pattern/2]).

:- use_module(library(semweb/rdf11)).
:- use_module(library(rdfs11)).
:- use_module(library(literals)).
:- use_module(library(utils)).

:- rdf_meta match(t),
            read_function(r, -, -, -),
            read_expression_list(o, -),
            read_expression(o, -),
            read_pattern(r, -),
            graph_triple(t),
            reset_graph(t, -),
            read_modifier(r, -, -),
            dont_infer(r, -).

triplesort([], []) :- !.
triplesort(Triples, SortedTriples) :-
  maplist(check_estimate, Triples, EstimatedTriples),
  keysort(EstimatedTriples, SortedTriples).

check_estimate(_-X, Y) :- !,
  check(X),
  estimate(X, Y).
check_estimate(X, Y) :-
  check(X),
  estimate(X, Y).

check(rdf(S,P,_)) :-
  check_subject_predicate(S, P).
check(rdf(S,P,_,G)) :-
  check_subject_predicate(S, P),
  maplist(var_or_atom, G).
check(rdfs(S,P,_)) :-
  check_subject_predicate(S, P).
check(not_pattern(X)) :-
  var_or_atom(X).
check(not(Triples,_)) :-
  maplist(check, Triples).
check(infer(S,P,_)) :-
  check_subject_predicate(S, P).
check(optional(X)) :-
  check(X).

check_subject_predicate(S, P) :-
  var_or_atom(S),
  var_or_atom(P),
  \+ rdf_is_bnode(P).

var_or_atom(G) :-
  var(G), !.
var_or_atom(G) :-
  atom(G).

estimate(rdf(S,P,O), C-rdf(S,P,O)) :-
  rdf_estimate_complexity(S, P, O, C).
estimate(rdf(S,P,O,G), C-rdf(S,P,O,G)) :-
  % TODO: sort G also
  rdf_estimate_complexity(S, P, O, C).
estimate(rdfs(S,P,O), C-rdfs(S,P,O)) :-
  rdfs_estimate_complexity(S, P, O, C).
estimate(not_pattern(X), C-not_pattern(X)) :-
  ( ground(X)
  -> C = 0
  ; C = a
  ).
estimate(not(X,Y), C-not(X,Y)) :-
  ( ground(Y)
  -> C = 1
  ; C = b
  ).
estimate(infer(S,P,O), C-infer(S,P,O)) :-
  ( ground(S)
  -> C = 1
  ; C = c
  ).
estimate(optional(X), d-C-optional(X)) :-
  estimate(X, C-X).

match(Triples) :-
  is_list(Triples),
  triplesort(Triples, SortedTriples),
  match_(SortedTriples).

match_([]) :- !.
match_([_-Triple]) :- !,
  match_triple(Triple, false).
match_([_-Triple|T]) :-
  match_triple(Triple, false),
  triplesort(T, SortedT),
  match_(SortedT).

match_triple(rdf(S,P,O,G), Optional) :-
  match_triple_graphs(rdf(S,P,O), G, Optional).
match_triple(rdf(S,P,OT), Optional) :-
  ( rdf(S,P,O) *-> object_to_term(O, OT) ; Optional ).
match_triple(rdfs(S,P,OT), Optional) :-
  ( rdfs(S,P,O) *-> object_to_term(O, OT) ; Optional ).
match_triple(not_pattern(S), _) :-
  ( ground(S)
  -> true
  ; instantiation_error(S)
  ),
  \+ rdfs(S, rdf:type, d:'Pattern').
match_triple(not(X,G), _) :-
  ( ground(G)
  -> true
  ; instantiation_error(G)
  ),
  triplesort(X, SortedX),
  \+ match_(SortedX).
match_triple(infer(S,P,OT), Optional) :-
  ( ground(S)
  -> rdfs(S, rdf:type, T)
  ; instantiation_error(T)
  ),
  rdf(T, d:getter, Getter),
  rdf(Getter, P, Function),
  ( call(Function, S, O) *-> object_to_term(O, OT) ; Optional ).
match_triple(optional(X), _) :-
  match_triple(X, true).

match_triple_graphs(_, [], _) :- !.
match_triple_graphs(rdf(S,P,OT), [H], Optional) :- !,
  ( rdf(S,P,O,H) *-> object_to_term(O, OT) ; Optional ).
match_triple_graphs(rdf(S,P,OT), [H|T], Optional) :-
  ( rdf(S,P,O,H) *-> object_to_term(O, OT) ; Optional ),
  match_triple_graphs(rdf(S,P,O), T, Optional).

read_function(IRI, Functor, Arity, Clauses) :-
  match([rdfs(IRI,rdf:type,d:'Function'),
         rdfs(IRI,d:define,Lambda),
         optional(rdfs(IRI,d:functor,FS))]),
  ( var(FS)
  -> Functor = IRI
  ; atom_string(Functor, FS)
  ),
  ( rdf_list(Lambda)
  -> rdf_list(Lambda, Lambdas),
     maplist(read_lambda(Functor, Arity), Lambdas, Clauses)
  ; read_lambda(Functor, Arity, Lambda, Clause),
    Clauses = [Clause]
  ).

read_lambda(Functor, Arity, IRI, Clause) :-
  ( rdfs(IRI, d:given, Args)
  -> ( read_expression_list(Args, A)
     -> Head =.. [Functor|A],
        length(A, Arity)
     ; read_expression(Args, A)
     -> Head =.. [Functor,A],
        Arity = 1
     )
  ; Head = Functor,
    Arity = 0
  ),
  ( rdfs(IRI, d:return, Exp)
  -> read_expression(Exp, E)
  ; E = true
  ),
  ( rdfs(IRI, d:where, Where)
  -> read_expression(Where, C),
     Clause0 = '==='(Head, where(E,C))
  ; rdfs(IRI, d:once, Once)
  -> read_expression(Once, C),
     Clause0 = '==='(Head,once(E,C))
  ; Clause0 = '==='(Head,E)
  ),
  varnumbers_names(Clause0, Clause, _).

read_expression_list(rdf:nil, []) :- !.
read_expression_list(Object, [H|T]) :-
  atom(Object),
  rdf(Object, rdf:first, First),
  rdf(Object, rdf:rest, Rest), !,
  read_expression(First, H),
  read_expression_list(Rest, T).

read_expression(Object, Expression) :-
  literal_to_term(Object, Term), !,
  ( Term == "[]"
  -> Expression = []
  ; Expression = Term
  ).
read_expression(Object, Expression) :-
  rdf_is_bnode(Object), !,
  read_bnode(Object, Expression).
read_expression(Object, Expression) :-
  rdfs(Object, rdf:type, d:'Pattern'), !,
  read_pattern(Object, Expression).
read_expression(Object, Expression) :-
  var_or_iri(Object, Expression).

read_bnode(Object, Expression) :-
  read_expression_list(Object, [FunctorS|Arguments]), !,
  ( string(FunctorS)
  -> atom_string(Functor, FunctorS)
  ; Functor = FunctorS
  ),
  ( Arguments \== [],
    compound(Functor)
  -> Expression =.. ['@',Functor|Arguments]
  ; Expression =.. [Functor|Arguments]
  ).
read_bnode(Object, Expression) :-
  read_pattern(Object, Expression).

var_or_iri(IRI, '$VAR'(V)) :-
  atom(IRI),
  rdf_equal(v:'', Prefix),
  atom_concat(Prefix, V0, IRI), !,
  ( V0 == '_'
  -> gensym('$ANON_', V)
  ; V = V0
  ).
var_or_iri(O, O).

read_pattern(IRI, Pattern) :-
  read_pattern_(IRI, _, _{type:rdf,optional:false}, Pattern, _).

read_pattern_(IRI, Id, Context, Pattern, []) :-
  phrase(read_pattern(IRI, Id, Context), Pattern).

read_pattern(IRI, Id, Context0) -->
  { findall(P-O, rdf(IRI, P, O), POs0),
    partition(graph_triple, POs0, GTs, POs),
    ( GTs == []
    -> Context = Context0
    ; ( reset_graph('-'(_,v:'_'), GTs)
      -> ( del_dict(graphs, Context0, _, Context)
         -> true
         ; Context = Context0
         )
      ; maplist(extract_graph, GTs, Graphs),
        put_dict(graphs, Context0, Graphs, Context)
      )
    )
  },
  read_triples(Id, POs, Context).

graph_triple('-'(d:graph,_)).

reset_graph(R, Gs) :-
  memberchk(R, Gs).

extract_graph(_-O, G) :-
  object_to_term(O, T),
  ( atom(T)
  -> G = T
  ; string(T),
    atom_string(G, T)
  ).

read_triples(_, [], _) --> !.

read_triples(Id, [P-O|T], Context) -->
  { rdf_equal(d:id, P), !,
    atom(O),
    \+ rdf_is_bnode(O),
    var_or_iri(O, Id)
  },
  read_triples(Id, T, Context).

read_triples(Id, [P-O|T], Context0) -->
  { read_modifier(P, Context0, Context) },
  !,
  read_pattern(O, Id0, Context),
  { ( var(Id0)
    -> Id = Id0
    ; true
    )
  },
  read_triples(Id, T, Context0).

read_triples(Id, [P-O|T], Context) -->
  { rdf_equal(d:not, P), !,
    read_pattern_(O, Id0, Context, Pattern, Vars),
    ( var(Id0)
    -> Id = Id0
    ; true
    )
  },
  [not(Pattern,Vars)],
  read_triples(Id, T, Context).

read_triples(Id, [P-O|T], Context) -->
  { rdf_is_bnode(O), !,
    var_or_iri(P, PV),
    triple_term(Id, PV, OV, Context, Term)
  },
  [Term],
  read_pattern(O, OV, Context),
  read_triples(Id, T, Context).

read_triples(Id, [P-O|T], Context) -->
  { var_or_iri(P, PV),
    var_or_iri(O, OV),
    triple_term(Id, PV, OV, Context, Term)
  },
  [Term],
  read_triples(Id, T, Context).

read_modifier(d:rdf, Context0, Context) :-
  put_dict(type, Context0, rdf, Context).

read_modifier(d:rdfs, Context0, Context) :-
  put_dict(type, Context0, rdfs, Context).

read_modifier(d:infer, Context0, Context) :-
  put_dict(type, Context0, infer, Context).

read_modifier(d:optional, Context0, Context) :-
  put_dict(optional, Context0, true, Context).

triple_term(S,P,O,Context,Term) :-
  get_dict(type, Context, Type),
  ( get_dict(graphs, Context, Graphs),
    Type == rdf
  -> Term0 =.. [Type,S,P,O,Graphs]
  ; \+ dont_infer(P, Type)
  -> Term0 =.. [Type,S,P,O]
  ; print_message(warning, invalid_pattern_triple(Type,S,P,O)),
    fail
  ),
  ( get_dict(optional, Context, true)
  -> Term = optional(Term0)
  ; Term = Term0
  ).

dont_infer(rdf:type, infer).
dont_infer(rdfs:subPropertyOf, infer).
dont_infer(rdfs:subClassOf, infer).
dont_infer(rdfs:domain, infer).
dont_infer(rdfs:range, infer).

:- multifile prolog:message/3.

prolog:message(invalid_pattern_triple(Type,S,P,O)) -->
  [ 'Invalid pattern triple ~w~@'-[Type,utils:debug_args(t(S,P,O))] ].
