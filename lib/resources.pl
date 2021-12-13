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
            graph_triple(t),
            reset_graph(t, -),
            variable(r, -, r),
            literal(o, -, -).

resource(Triples) :-
  resource(Triples, false).

resource(Triples, Optional) :-
  triplesort(Triples, SortedTriples),
  match(SortedTriples, Optional).

triplesort([], []) :- !.
triplesort(Triples, SortedTriples) :-
  maplist(check_type, Triples, CheckedTriples),
  maplist(estimate, CheckedTriples, EstimatedTriples),
  keysort(EstimatedTriples, SortedTriples).

check_type(rdf(S,P,O), rdf(S,P,O)) :- !,
  var_or_atom(S),
  var_or_atom(P), \+ rdf_is_bnode(P).
check_type(_-rdf(S,P,O), T) :- !,
  check_type(rdf(S,P,O), T).
check_type(rdf(S,P,O,G), rdf(S,P,O,G)) :- !,
  var_or_atom(S),
  var_or_atom(P), \+ rdf_is_bnode(P),
  maplist(var_or_atom, G).
check_type(_-rdf(S,P,O,G), T) :- !,
  check_type(rdf(S,P,O,G), T).
check_type(rdfs(S,P,O), rdfs(S,P,O)) :- !,
  var_or_atom(S),
  var_or_atom(P), \+ rdf_is_bnode(P).
check_type(_-rdfs(S,P,O), T) :- !,
  check_type(rdfs(S,P,O), T).
check_type(optional(X), optional(X)) :- !.
check_type(_-optional(X), optional(X)) :- !.

var_or_atom(G) :-
  ( var(G) ; atom(G) ).

estimate(rdf(S,P,O), C-rdf(S,P,O)) :-
  rdf_estimate_complexity(S, P, O, C).
estimate(rdf(S,P,O,G), C-rdf(S,P,O,G)) :-
  % TODO: sort G also
  rdf_estimate_complexity(S, P, O, C).
estimate(rdfs(S,P,O), C-rdfs(S,P,O)) :-
  rdfs_estimate_complexity(S, P, O, C).
estimate(optional(X), a-optional(X)).

match([], _) :- !.
match([_-Triple], Optional) :- !,
  match_triple(Triple, Optional).
match([_-Triple|T], Optional) :- !,
  match_triple(Triple, Optional),
  triplesort(T, SortedT),
  match(SortedT, Optional).

match_triple(optional(X), _) :-
  resource(X, true).
match_triple(rdf(S,P,O,G), Optional) :-
  match_triple(rdf(S,P,O), G, Optional).
match_triple(rdf(S,P,O), Optional) :-
  ( rdf(S,P,O) *-> true ; Optional ).
match_triple(rdfs(S,P,O), Optional) :-
  ( rdfs(S,P,O) *-> true ; Optional ).

match_triple(_, [], _) :- !.
match_triple(X, [H|T], Optional) :-
  ( call(X, H) *-> true ; Optional ),
  match_triple(X, T, Optional).

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
  -> ( variable(IRI, '$VAR'(V), v:'')
     -> Expression = '$VAR'(V)
     ; ( resource([ rdfs(IRI,rdf:type,d:'Function'),
                    rdfs(IRI,d:functor,^^(FS,_)) ])
       -> atom_string(Expression, FS)
       ; read_application(IRI, Expression)
       -> true
       ; rdf_is_bnode(IRI)
       -> phrase(read_pattern(IRI, _, _{type:rdf}), Triples0),
          varnumbers_names(Triples0, Triples1, Vars),
          ground_not(Triples1, Expression),
          varnumbers_vars(Vars)
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

varnumbers_vars([]) :- !.
varnumbers_vars([Var=V|T]) :-
  V = '$VAR'(Var),
  varnumbers_vars(T).

variable(IRI, '$VAR'(V), Prefix) :-
  atom(IRI),
  atom_concat(Prefix, V0, IRI), !,
  ( V0 == '_'
  -> gensym('$ANON_', V)
  ; V = V0
  ).
variable(O, O, _).

literal(^^(S, xsd:string), S, string) :- !.
literal(@(S, _), S, string) :- !.
literal(^^(N, _), N, number) :-
  number(N).

read_application(rdf:nil, []) :- !.
read_application(IRI, Application) :-
  resource([rdf(IRI,rdf:first,First),
            rdf(IRI,rdf:rest,Rest)]),
  read_args(Rest, Args),
  read_expression(First, Expression),
  ( compound(Expression)
  -> Application =.. ['@',Expression|Args]
  ; Application =.. [Expression|Args]
  ).

read_args(rdf:nil, []) :- !.
read_args(IRI, [H|T]) :-
  resource([rdf(IRI,rdf:first,First),
            rdf(IRI,rdf:rest,Rest)]),
  read_expression(First, H),
  read_args(Rest, T).

read_pattern(IRI, Id, Context0) -->
  { findall(P-O, resource([rdf(IRI,P,O)]), POs0),
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
  literal(L, S, string),
  atom_string(G, S).

read_triples(_, [], _) --> !.

read_triples(Id, [P-O|T], Context) -->
  { rdf_equal(d:id, P), !,
    atom(O),
    \+ rdf_is_bnode(O),
    variable(O, Id, v:'')
  },
  read_triples(Id, T, Context).

read_triples(Id, [P-O|T], Context0) -->
  { rdf_global_id(d:Type, P),
    memberchk(Type, [rdf,rdfs]),
    rdf_is_bnode(O),
    put_dict(type, Context0, Type, Context)
  }, !,
  read_pattern(O, Id0, Context),
  { ( var(Id0)
    -> Id = Id0
    ; true
    )
  },
  read_triples(Id, T, Context).

read_triples(Id, [P-O|T], Context) -->
  { rdf_equal(d:not, P),
    rdf_is_bnode(O), !,
    phrase(read_pattern(O, Id0, Context), Not),
    ( var(Id0)
    -> Id = Id0
    ; true
    )
  },
  [not(Not)],
  read_triples(Id, T, Context).

read_triples(Id, [P-O|T], Context) -->
  { rdf_equal(d:optional, P),
    rdf_is_bnode(O), !,
    phrase(read_pattern(O, Id0, Context), Optional),
    ( var(Id0)
    -> Id = Id0
    ; true
    )
  },
  [optional(Optional)],
  read_triples(Id, T, Context).

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

triple_term(S,P,O,Context,Term) :-
  get_dict(type, Context, Type),
  ( get_dict(graphs, Context, Graphs)
  -> Type == rdf,
     Term =.. [Type,S,P,O,Graphs]
  ; Term =.. [Type,S,P,O]
  ).

ground_not(Triples0, Triples) :-
  term_variables(Triples0, Vs),
  term_singletons(Triples0, Ss),
  ord_subtract(Vs, Ss, Vars),
  ground_not_(Triples0, Triples, Vars, false).

ground_not_([], [], _, _) :- !.
ground_not_([not(H)|T], Ts, Vars, Not) :- !,
  ( Not -> Not1 = false ; Not1 = true ),
  ground_not_(H, H2, Vars, Not1),
  append(H2, T2, Ts),
  ground_not_(T, T2, Vars, Not).
ground_not_([H|T], [H|T2], Vars, false) :- !,
  ground_not_(T, T2, Vars, false).
ground_not_([H|T], [not(H,G)|T2], Vars, true) :-
  term_variables(H, Vs),
  ord_intersection(Vs, Vars, G),
  ground_not_(T, T2, Vars, true).
