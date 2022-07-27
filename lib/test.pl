:- module(test, []).

:- use_module(library(semweb/rdf11)).
:- use_module(library(rdfs11)).
:- use_module(library(resources)).

:- rdf_meta rf(r), ma(r).

rf(IRI) :-
  read_function(IRI, Fu, Ar, Fs),
  assertz(lambda:fun(user:Fu/Ar)),
  forall(
    member(F, Fs),
    ( term_expansion(F, Es),
      maplist(user:assertz, Es),
      succ(Ar, Ar2),
      export(user:Fu/Ar2),
      forall(
        member(E, Es),
        ( portray_clause(E),
          nl
        )
      )
    )
  ).

ma(IRI) :-
  read_expression(IRI, Pattern0),
  varnumbers_names(Pattern0, Pattern, Vs),
  print_term((Pattern,Vs), []), nl, nl,
  match(Pattern),
  print_term(Vs, []), nl.
