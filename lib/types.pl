:- module(types, [ type//4,
                   op(500, yfx, $) ] ).

kind(var(Z), K, KC) :- first(Z:K0, KC), instantiate(K0, K).
kind(F$G, K, KC) :- kind(F, K0->K, KC), kind(G, K0, KC).
kind(A->B, o, KC) :- kind(A, o, KC), kind(B, o, KC).

type(var(X), T, KC, C) --> { first(X:T0, C) }, inst_type(T0, T, KC).
type(lam(X,E), A->B, KC, C) --> type(E, B, KC, [X:mono(A)|C]), [kind(A->B, o, KC)].
type(X$Y, B, KC, C) --> type(X, A->B, KC, C), type(Y, A, KC, C).
type(let(X=E0,E1), T, KC, C) --> type(E0, A, KC, C), type(E1, T, KC, [X:poly(C,A)|C]).

first(K:V, [K:V1|_]) :- unify_with_occurs_check(V, V1), !.
first(K:V, [_|Xs]) :- first(K:V, Xs).

instantiate(mono(T), T) :- !.
instantiate(poly(C,T0), T) :- copy_term(t(C,T0), t(C,T)).

inst_type(poly(C,T0), T, KC) --> { copy_term(t(C,T0), t(C,T1)),
                                   free_variables(T0, Xs0),
                                   free_variables(T1, Xs1)
                                 },
                                 samekinds(KC, Xs0, Xs1),
                                 { T = T1 }.
inst_type(mono(T), T, _) --> [].

samekinds(KC, [X|Xs], [Y|Ys]) --> { X \== Y },
                                  [kind(X, K, KC), kind(Y, K, KC)],
                                  samekinds(KC, Xs, Ys).
samekinds(KC, [X|Xs], [X|Ys]) --> samekinds(KC, Xs, Ys).
samekinds(_, [], []) --> [].

variablize(var(X)) :- gensym(t, X).

infer_type(KC, C, E, T) :-
  phrase(type(E, T, KC, C), Gs0),
  copy_term(Gs0, Gs),
  bagof(Ty, member(kind(Ty, _, _), Gs), Tys),
  free_variables(Tys, Xs),
  maplist(variablize, Xs),
  bagof(A:_, member(var(A), Xs), KC1),
  appendKC(Gs, KC1, Gs1),
  maplist(call, Gs1).

appendKC([], _, []) :- !.
appendKC([kind(X, K, KC)|Gs], KC1, [kind(X, K, KC2)|Gs1]) :-
  append(KC1, KC, KC2),
  appendKC(Gs, KC1, Gs1).
