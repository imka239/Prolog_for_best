print_tree(null) :- !.
print_tree(tree(K, V, L, R)) :-
	print_tree(L),
	write(K), write(V), nl,
	print_tree(R).

mega_build([], [], null) :- write(1), nl, !.
mega_build([], [(K1, V1)], tree(K1, V1, null, null)):- write(2), nl, !.
mega_build(T, [(K, V) | Map], tree(K, V, L, R)):-
    append(T, [(K, V)], NewT),
    length(NewT, L1), length(Map, L2), L1 >= L2, write(5), nl,
    mega_build([], T, L),
    mega_build([], Map, R), !.
mega_build(T, [(K1, V1) | Map], tree(K, V, L, R)):-
    append(T, [(K1, V1)], NewT),
    write(NewT), nl,
    mega_build(NewT, Map, tree(K, V, L, R)).

split(null, _, null, null) :- !.
split(tree(K, V, P, L, R), Val, tree(K, V, P, L, L1), Right) :-
    K < Val, split(R, Val, L1, Right),!.
split(tree(K, V, P, L, R), Val, Left, tree(K, V, P, R1, R)) :-
    K >= Val, split(L, Val, Left, R1),!.

merge(null, T, T) :- !.
merge(T, null, T) :- !.
merge(tree(K1, V1, P1, L1, R1), tree(K2, V2, P2, L2, R2), tree(K1, V1, P1, L1, Right)) :-
    P1 < P2, merge(R1, tree(K2, V2, P2, L2, R2), Right),!.
merge(tree(K1, V1, P1, L1, R1), tree(K2, V2, P2, L2, R2), tree(K2, V2, P2, Left, R2)) :-
    P1 >= P2, merge(tree(K1, V1, P1, L1, R1), L2, Left),write(R2),!.

map_remove(Tree, Key, NewAns) :-
    split(Tree, Key, Left1, Right1),
    split(Right1, Key + 1, _, Right2),
    merge(Left1, Right2, NewAns),!.
map_put(null, K, V, tree(K, V, P, null, null)) :- rand_int(1000000000, P), !.
map_put(Tree, Key, Value, NewAns) :-
    split(Tree, Key, Left1, Right1),
    split(Right1, Key + 1, _, Right2),
    rand_int(1000000000, P),
    merge(Left1, tree(Key, Value, P, null, null), Merg),
    merge(Merg, Right2, NewAns), !.

build([], Tree, Tree) :- !.
build([(K, V)], Tree, Ans) :- map_put(Tree, K, V, Ans),!.
build([(K, V) | Map], Tree, Ans) :-
    map_put(Tree, K, V, A),
    build(Map, A, Ans),!.

tree_build(Map, Tree) :- %mega_build([], Map, Tree).
    build(Map, null, Tree),!.

map_get(tree(K, V, _, _, _), K, V) :- !.
map_get(tree(Key, _, _, L, _), K, V) :-
    K < Key, map_get(L, K, V),!.
map_get(tree(Key, _, _, _, R), K, V) :-
    K > Key, map_get(R, K, V),!.

map_next(null, K, K, null).

map_next(tree(K, _, _, R), Key, K2, V2) :-
    K < Key, map_next(R, Key, K2, V2),!.

map_next(tree(K, V, L, _), Key, K, V) :-
    K > Key, map_next(L, Key, K3, _), (K < K3 ; K3 == Key), !.

map_next(tree(K, _, L, _), Key, K3, V3) :-
    K > Key, map_next(L, Key, K3, V3), K >= K3, !.

map_next(tree(K, _, _, R), K, K3, V3) :-
    map_next(R, K, K3, V3),!.

map_prev(null, K, K, null).

map_prev(tree(K, _, L, _), Key, K2, V2) :-
    K > Key, map_prev(L, Key, K2, V2),!.

map_prev(tree(K, V, _, R), Key, K, V) :-
    K < Key, map_prev(R, Key, K3, _), (K > K3 ; K3 == Key), !.

map_prev(tree(K, _, L, _), Key, K3, V3) :-
    K < Key, map_prev(L, Key, K3, V3), K =< K3, !.

map_prev(tree(K, _, L, _), K, K3, V3) :-
    map_prev(L, K, K3, V3),!.