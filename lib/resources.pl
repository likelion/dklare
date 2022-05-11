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
            modifier(r, -),
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
  \+ rdfschk(S, rdf:type, d:'Pattern').
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
  ( rdfschk(IRI, d:given, Args)
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
  ( rdfschk(IRI, d:return, Exp)
  -> read_expression(Exp, E)
  ; E = true
  ),
  ( rdfschk(IRI, d:where, Where)
  -> read_expression(Where, C),
     Clause0 = '==='(Head, where(E,C))
  ; rdfschk(IRI, d:once, Once)
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
  rdfschk(Object, rdf:type, d:'Pattern'),
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

var_or_iri(IRI, V) :-
  variable(IRI, V), !.
var_or_iri(IRI, IRI).

variable(IRI, V) :-
  atom(IRI),
  rdf_equal(v:'', Prefix),
  atom_concat(Prefix, V0, IRI),
  ( V0 == '_'
  -> true
  ; V = '$VAR'(V0)
  ).

var_or_object(IRI, V), [Context] --> [Context0],
  { ( variable(IRI, V)
    -> ( var(V)
       -> gensym('$ANON_', V1),
          V = '$VAR'(V1),
          Context = Context0
       ; get_dict(vars, Context0, Vars0),
         union([V], Vars0, Vars),
         put_dict(vars, Context0, Vars, Context)
       )
    ; V = IRI,
      Context = Context0
    )
  }.

read_pattern(IRI, Pattern) :-
  phrase(
    read_pattern(IRI, _, [], Pattern),
    [_{type:rdf,vars:[]}],
    _
  ).

read_pattern(IRI, Id, Pattern0, Pattern) -->
  lookup(Context0),
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
  set_context(Context),
  read_triples(Id, POs, Pattern0, Pattern).

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

attr_unify_hook(_, _).

read_triples(_, [], Pattern, Pattern) --> !.

read_triples(Id, [P-O|T], Pattern0, Pattern) -->
  { rdf_equal(d:id, P), !,
    atom(O),
    \+ rdf_is_bnode(O)
  },
  var_or_object(O, Id),
  read_triples(Id, T, Pattern0, Pattern).

read_triples(Id, [P-O|T], Pattern0, Pattern) -->
  { modifier(P, Modifier) }, !,
  lookup(Context),
  set_context(Context.put(Modifier)),
  { put_attr(Id0, resources, Id) },
  read_pattern(O, Id0, Pattern0, Pattern1),
  { del_attr(Id0, resources) },
  set_context(Context),
  { ( var(Id0)
    -> Id = Id0
    ; true
    )
  },
  read_triples(Id, T, Pattern1, Pattern).

read_triples(Id, [P-O|T], Pattern0, [not(Not,Ground)|Pattern]) -->
  { rdf_equal(d:not, P) }, !,
  read_triples(Id, T, Pattern0, Pattern),
  lookup(Context1),
  set_context(Context1.put(vars, [])),
  read_pattern(O, Id0, [], Not),
  lookup(Context2),
  { get_dict(vars, Context1, Vars10),
    get_dict(vars, Context2, Vars20),
    ( var(Id0)
    -> Id = Id0,
       Vars2 = [Id|Vars20],
       ( var(Id)
       -> Vars1 = [Id|Vars10]
       ; Vars1 = Vars10
       )
    ; Vars2 = Vars20,
      Vars1 = Vars10
    ),
    intersection(Vars1, Vars2, Ground),
    union(Vars2, Vars1, Vars)
  },
  set_context(Context1.put(vars, Vars)).

read_triples(Id, [P-O|T], Pattern0, Pattern) -->
  { rdf_is_bnode(O) }, !,
  var_or_object(P, PV),
  triple_term(Id, PV, OV, Term),
  read_pattern(O, OV, [Term|Pattern0], Pattern1),
  read_triples(Id, T, Pattern1, Pattern).

read_triples(Id, [P-O|T], Pattern0, [Term|Pattern2]) -->
  var_or_object(P, PV),
  var_or_object(O, OV),
  triple_term(Id, PV, OV, Term),
  read_triples(Id, T, Pattern0, Pattern1),
  { ( get_attr(Id, resources, Id0)
    -> ( \+ contains_var(not_pattern(Id0), [Pattern1])
       -> Pattern2 = [not_pattern(Id0)|Pattern1]
       ; Pattern2 = Pattern1
       )
    ; ( \+ contains_var(not_pattern(Id), [Pattern1])
      -> Pattern2 = [not_pattern(Id)|Pattern1]
      ; Pattern2 = Pattern1
      )
    )
  }.

intersection([], _, []) :- !.
intersection([H|T], A, [H|T2]) :-
  contains_var(H, A), !,
  intersection(T, A, T2).
intersection([_|T], A, T2) :-
  intersection(T, A, T2).

union([], A, A) :- !.
union([H|T], A, B) :-
  contains_var(H, A), !,
  union(T, A, B).
union([H|T], A, [H|B]) :-
  union(T, A, B).

modifier(d:rdf, _{type:rdf}).
modifier(d:rdfs, _{type:rdfs}).
modifier(d:infer, _{type:infer}).
modifier(d:optional, _{optional:true}).

triple_term(S,P,O,Term) -->
  lookup(Context),
  { get_dict(type, Context, Type),
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
    )
  }.

dont_infer(rdf:type, infer).
dont_infer(rdfs:subPropertyOf, infer).
dont_infer(rdfs:subClassOf, infer).
dont_infer(rdfs:domain, infer).
dont_infer(rdfs:range, infer).

:- multifile prolog:message/3.

prolog:message(invalid_pattern_triple(Type,S,P,O)) -->
  [ 'Invalid pattern triple ~w~@'-[Type,utils:debug_args(t(S,P,O))] ].
