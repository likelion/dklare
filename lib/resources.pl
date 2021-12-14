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

:- module(resources, [resource/1,
                      read_function/4,
                      read_expression/2,
                      read_pattern/2]).

:- use_module(library(semweb/rdf11)).
:- use_module(library(rdfs11)).

:- rdf_meta resource(t),
            read_function(r, -, -, -),
            read_expression(r, -),
            read_application(r, -),
            read_args(r, -),
            read_pattern(r, -),
            graph_triple(t),
            reset_graph(t, -),
            variable(r, -, r),
            literal(o, -, -),
            read_modifier(r, -, -).

resource(Triples) :-
  triplesort(Triples, SortedTriples),
  match(SortedTriples).

triplesort([], []) :- !.
triplesort(Triples, SortedTriples) :-
  maplist(check_type, Triples, CheckedTriples),
  maplist(estimate, CheckedTriples, EstimatedTriples),
  keysort(EstimatedTriples, SortedTriples).

check_type(rdf(S,P,O), rdf(S,P,O)) :-
  check_subject_predicate(S, P).
check_type(rdf(S,P,O,G), rdf(S,P,O,G)) :-
  check_subject_predicate(S, P),
  maplist(var_or_atom, G).
check_type(rdfs(S,P,O), rdfs(S,P,O)) :-
  check_subject_predicate(S, P).
check_type(not(X,Y), not(X,Y)) :-
  check_type(X, X).
check_type(not_pattern(X), not_pattern(X)).
check_type(optional(X), optional(X)) :-
  check_type(X, X).
check_type(_-X, Y) :-
  check_type(X, Y).

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
estimate(not(X,Y), C-not(X,Y)) :-
  ( ground(Y)
  -> C = 1
  ; C = n
  ).
estimate(not_pattern(X), C-not_pattern(X)) :-
  ( ground(X)
  -> C = 0
  ; C = m
  ).
estimate(optional(X), o-C-optional(X)) :-
  estimate(X, C-X).

match([]) :- !.
match([_-Triple]) :- !,
  match_triple(Triple, false).
match([_-Triple|T]) :-
  match_triple(Triple, false),
  triplesort(T, SortedT),
  match(SortedT).

match_triple(not(X,G), _) :-
  ( ground(G)
  -> true
  ; instantiation_error(G)
  ),
  \+ match_triple(X, false).
match_triple(not_pattern(S), _) :-
  \+ rdfs(S, rdf:type, d:'Pattern').
match_triple(optional(X), _) :-
  match_triple(X, true).
match_triple(rdf(S,P,O,G), Optional) :-
  match_triple(rdf(S,P,O), G, Optional).
match_triple(rdf(S,P,O), Optional) :-
  ( rdf(S,P,O) *-> true ; Optional ).
match_triple(rdfs(S,P,O), Optional) :-
  ( rdfs(S,P,O) *-> true ; Optional ).

match_triple(_, [], _) :- !.
match_triple(rdf(S,P,O), [H], Optional) :- !,
  ( rdf(S,P,O,H) *-> true ; Optional ).
match_triple(rdf(S,P,O), [H|T], Optional) :-
  ( rdf(S,P,O,H) *-> true ; Optional ),
  match_triple(rdf(S,P,O), T, Optional).

read_function(IRI, Functor, Arity, Clauses) :-
  resource([rdfs(IRI,rdf:type,d:'Function'),
            rdfs(IRI,d:define,Lambda),
            optional(rdfs(IRI,d:functor,FS))]),
  ( literal(FS, Functor, string)
  -> true
  ; Functor = IRI
  ),
  ( rdf_list(Lambda)
  -> rdf_list(Lambda, Lambdas),
     maplist(read_lambda(Functor, Arity), Lambdas, Clauses)
  ; read_lambda(Functor, Arity, Lambda, Clause),
    Clauses = [Clause]
  ).

read_lambda(Functor, Arity, IRI, Clause) :-
  ( rdfs(IRI, d:given, Args)
  -> read_args(Args, A),
     Head =.. [Functor|A],
     length(A, Arity)
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
     Clause0 = '==='(Head,if(E,C))
  ; Clause0 = '==='(Head,E)
  ),
  varnumbers_names(Clause0, Clause, _).

read_expression(IRI, Expression) :-
  ( atom(IRI)
  -> ( variable(IRI, '$VAR'(V), v:'')
     -> Expression = '$VAR'(V)
     ; read_application(IRI, Expression)
     -> true
     ; read_pattern(IRI, Expression)
     -> true
     ; Expression = IRI
     )
  ; ( literal(IRI, FS, string)
    -> ( FS == '[]'
       -> Expression = []
       ; Expression = FS
       )
    ; literal(IRI, Expression, number)
    )
  ).

variable(IRI, '$VAR'(V), Prefix) :-
  atom(IRI),
  atom_concat(Prefix, V0, IRI), !,
  ( V0 == '_'
  -> gensym('$ANON_', V)
  ; V = V0
  ).
variable(O, O, _).

literal(^^(S, xsd:string), A, string) :- !,
  string(S),
  atom_string(A, S).
literal(@(S, _), A, string) :- !,
  string(S),
  atom_string(A, S).
literal(^^(N, _), N, number) :-
  number(N).

read_application(rdf:nil, []) :- !.
read_application(IRI, Application) :-
  rdf(IRI, rdf:first, First),
  rdf(IRI, rdf:rest, Rest),
  read_expression(First, Expression),
  read_args(Rest, Args),
  ( compound(Expression)
  -> Application =.. ['@',Expression|Args]
  ; Application =.. [Expression|Args]
  ).

read_args(rdf:nil, []) :- !.
read_args(IRI, [H|T]) :-
  rdf(IRI, rdf:first, First),
  rdf(IRI, rdf:rest, Rest),
  !,
  read_expression(First, H),
  read_args(Rest, T).
read_args(IRI, [E]) :-
  read_expression(IRI, E).

read_pattern(IRI, Pattern) :-
  once((
    rdf_is_bnode(IRI)
  ; rdfs(IRI, rdf:type, d:'Pattern')
  )),
  phrase(read_pattern(IRI, _, _{type:rdf,not:false,optional:false}), Triples0),
  varnumbers_names(Triples0, Triples1, Vars),
  ground_not(Triples1, Pattern),
  varnumbers_vars(Vars).

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

extract_graph(_-L, G) :-
  atom(L), !,
  variable(L, G, v:'').
extract_graph(_-L, G) :-
  literal(L, G, string).

read_triples(_, [], _) --> !.

read_triples(Id, [P-O|T], Context) -->
  { rdf_equal(d:id, P), !,
    atom(O),
    \+ rdf_is_bnode(O),
    variable(O, Id, v:'')
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
  { rdf_is_bnode(O), !,
    variable(P, PV, v:''),
    triple_term(Id, PV, OV, Context, Term)
  },
  [Term],
  read_pattern(O, OV, Context),
  read_triples(Id, T, Context).

read_triples(Id, [P-O|T], Context) -->
  { variable(P, PV, v:''),
    variable(O, OV, v:''),
    triple_term(Id, PV, OV, Context, Term)
  },
  [Term],
  read_triples(Id, T, Context).

read_modifier(d:rdf, Context0, Context) :-
  put_dict(type, Context0, rdf, Context).

read_modifier(d:rdfs, Context0, Context) :-
  put_dict(type, Context0, rdfs, Context).

read_modifier(d:optional, Context0, Context) :-
  put_dict(optional, Context0, true, Context).

read_modifier(d:not, Context0, Context) :-
  get_dict(not, Context0, Not),
  not(Not, Not1),
  put_dict(not, Context0, Not1, Context).

not(true, false).
not(false, true).

triple_term(S,P,O,Context,Term) :-
  get_dict(type, Context, Type),
  ( get_dict(graphs, Context, Graphs),
    Type == rdf
  -> Term0 =.. [Type,S,P,O,Graphs]
  ; Term0 =.. [Type,S,P,O]
  ),
  ( get_dict(not, Context, true)
  -> Term1 = not(Term0)
  ; Term1 = Term0
  ),
  ( get_dict(optional, Context, true)
  -> Term = optional(Term1)
  ; Term = Term1
  ).

ground_not(Triples0, Triples) :-
  term_variables(Triples0, Vs),
  term_singletons(Triples0, Ss),
  ord_subtract(Vs, Ss, Vars),
  ground_not_(Triples0, Triples1, Vars, [], Checks),
  append(Triples1, Checks, Triples).

ground_not_([], [], _, C, C) :- !.
ground_not_([not(H)|T], [not(H,G)|T2], Vars, C0, C) :- !,
  term_variables(H, Vs),
  ord_intersection(Vs, Vars, G),
  ground_not_(T, T2, Vars, C0, C).
ground_not_([H|T], [H|T2], Vars, C0, C) :- !,
  H =.. [F,S|_],
  memberchk(F, [rdf,rdfs]),
  ( memberchk(not_pattern(S), C0)
  -> C1 = C0
  ; C1 = [not_pattern(S)|C0]
  ),
  ground_not_(T, T2, Vars, C1, C).
ground_not_([H|T], [H|T2], Vars, C0, C) :- !,
  ground_not_(T, T2, Vars, C0, C).

varnumbers_vars([]) :- !.
varnumbers_vars([Var=V|T]) :-
  V = '$VAR'(Var),
  varnumbers_vars(T).
