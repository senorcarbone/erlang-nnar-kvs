-module(merkle_lazy).
-include_lib("eunit/include/eunit.hrl").
-export([empty/0, diff/2, add/2, remove/2 , roothash/1, test/0]).
-record(mtree, {roothash=0, hashstore=ordsets:new()}).

%%nlas
%%returns an empty mtree
empty() ->
	#mtree{}.

diff(Mtree=#mtree{}, Mtree2=#mtree{}) ->
	Missing = ordsets:subtract(Mtree2#mtree.hashstore, Mtree#mtree.hashstore),
	Addit = ordsets:subtract(Mtree#mtree.hashstore, Mtree2#mtree.hashstore),
	{ordsets:to_list(Missing), ordsets:to_list(Addit)}.

add(Element, Mtree=#mtree{}) ->
	UpdatedSet = ordsets:add_element(Element, Mtree#mtree.hashstore),
	UpdatedHash = recompute(ordsets:to_list(UpdatedSet)),
	#mtree{roothash=UpdatedHash, hashstore=UpdatedSet}.

remove(Element, Mtree=#mtree{}) ->
	UpdatedSet = ordsets:del_element(Element, Mtree#mtree.hashstore),
	UpdatedHash = recompute(ordsets:to_list(UpdatedSet)),
	#mtree{roothash=UpdatedHash, hashstore=UpdatedSet}.

roothash(Mtree=#mtree{}) ->
	Mtree#mtree.roothash.

recompute(Hashes) ->
	recompute1(Hashes, []).

recompute1([], [RootHash]) ->
	RootHash;
recompute1([], Acc) ->
	recompute1(Acc, []);
recompute1([N1,N2 | Tail], Acc) ->
	recompute1(Tail , Acc++[merge(N1,N2)]);
recompute1([N], Acc) ->
	recompute1([],Acc++[crypto:sha(term_to_binary(N))]).

merge(Hash1, Hash2) ->
	crypto:sha(term_to_binary([Hash1,Hash2])).

test() ->
	Merkle1 = merkle_lazy:add(123, merkle_lazy:add(321, merkle_lazy:empty())),
	Merkle2 = merkle_lazy:add(321, merkle_lazy:add(123, merkle_lazy:empty())),
	?assertEqual(merkle_lazy:roothash(Merkle1), merkle_lazy:roothash(Merkle2)),
	Merkle3 = merkle_lazy:add(234, Merkle2),
	?assertNotEqual(merkle_lazy:roothash(Merkle1), merkle_lazy:roothash(Merkle3)),
	Merkle4 = merkle_lazy:remove(123,Merkle1),
	?assertNotEqual(merkle_lazy:roothash(Merkle1), merkle_lazy:roothash(Merkle4)),
	?assertEqual({[123,234], []}, merkle_lazy:diff(Merkle4,Merkle3)),
	?assertEqual({[],[123,234]}, merkle_lazy:diff(Merkle3,Merkle4)),
	ok.
