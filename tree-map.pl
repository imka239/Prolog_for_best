print_tree(null) :- !.
print_tree(tree(K, V, L, R)) :-
	print_tree(L),
	write(K), write(V), nl,
	print_tree(R).

mega_build([], [], null) :- write(1), nl, !.
mega_build([], [(K1, V1)], tree(K1, V1, P1, null, null)):- write(2), nl, !.
mega_build(T, [(K, V) | Map], tree(K, V, P, L, R)):-
    append(T, [(K, V)], NewT),
    length(NewT, L1), length(Map, L2), L1 >= L2, write(5), nl, rand_int(1000000, P),
    mega_build([], T, L),
    mega_build([], Map, R), !.
mega_build(T, [(K1, V1) | Map], tree(K, V, P, L, R)):-
    append(T, [(K1, V1)], NewT),
    write(NewT), nl,
    mega_build(NewT, Map, tree(K, V, P, L, R)).

split(null, _, null, null, null) :- !.
split(tree(K, V, P, L, R), Val, tree(K, V, P, L, L1), Mid, Right) :-
    K < Val, split(R, Val, L1, Mid, Right), write(Mid),!.
split(tree(K, V, P, L, R), Val, Left, Mid, tree(K, V, P, R1, R)) :-
    K > Val, split(L, Val, Left, Mid, R1),!.
split(tree(K, V, P, L, R), K, L, tree(K, V, P, null, null), R) :- !.


merge(null, T, T) :- !.
merge(T, null, T) :- !.
merge(tree(K1, V1, P1, L1, R1), tree(K2, V2, P2, L2, R2), tree(K1, V1, P1, L1, Right)) :-
    P1 < P2, merge(R1, tree(K2, V2, P2, L2, R2), Right),!.
merge(tree(K1, V1, P1, L1, R1), tree(K2, V2, P2, L2, R2), tree(K2, V2, P2, Left, R2)) :-
    P1 >= P2, merge(tree(K1, V1, P1, L1, R1), L2, Left),write(R2),!.

map_remove(Tree, Key, NewAns) :-
    split(Tree, Key, Left1, Mid, Right1),
    merge(Left1, Right1, NewAns),!.

map_put(null, K, V, tree(K, V, P, null, null)) :- rand_int(1000000000, P), !.
map_put(Tree, Key, Value, NewAns) :-
    split(Tree, Key, Left1, _, Right1),
    rand_int(1000000000, P),
    merge(Left1, tree(Key, Value, P, null, null), Merg),
    merge(Merg, Right1, NewAns), !.

build([], Tree, Tree) :- !.
build([(K, V) | Map], Tree, Ans) :-
    map_put(Tree, K, V, A),
    build(Map, A, Ans),!.

super_build([], Tree, Tree) :- !.
super_build([(K, V) | Map], Tree, NewTree) :-
    rand_int(1000000000, P),
    merge(Tree, tree(K, V, P, null, null), Tr),
    super_build(Map, Tr, NewTree),!.


tree_build(Map, Tree) :- %mega_build([], Map, Tree), !.
                         %build(Map, null, Tree), !.
                         super_build(Map, null, Tree),!.
                         %normalize(Map, [], Ans),
                         %decart(Ans, [], null, Tree), !.

map_get(Tree, K, V) :- split(Tree, K, _, tree(K, V, _, _, _), _).

map_replace(Tree, K, V, Tree) :-
    split(Tree, K, L1, null, R1), !.

map_replace(Tree, K, V, Ans) :-
    split(Tree, K, L1, tree(K1, V1, P1, L3, R3), R1),
    merge(L1, tree(K, V, P1, L3, R3), Answer),
    merge(Answer, R1, Ans),!.