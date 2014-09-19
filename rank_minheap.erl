%%----------------------------------------------------
%% @desc: minheap for rnaking
%% @author chengcheng<cyoucmi@gmail.com>
%% @date: 2014/9/18
%%----------------------------------------------------

%% MinHeap = { H, list, gb_trees}

-module(rank_minheap).
-export([new/1, insert/4]).

-include("common.hrl").

new(H)->
    G = gb_trees:empty(),
    IndexSize = 2 bsl H - 1,
    Tuple = list_to_tuple(lists:duplicate(IndexSize, {0, 0, 0})),
    {H, G, Tuple}.

insert(Key, Value, UniqueId, Heap)->
    {H, G, Tuple} = Heap,
    HeapTop = element(1, Tuple),
    insert_1(Key, Value, UniqueId, G, HeapTop, H, Tuple).

%% heap top bigger than inser key
insert_1(Key, _, _, G, {HeapTopKey, _, _}, H, Tuple) when HeapTopKey >= Key ->
    {H, G, Tuple};

insert_1(Key, Value, UniqueId, G, {_HeapTopKey, _HeapTopValue, HeapTopUniqueId}, H, Tuple)->
    case gb_trees:lookup({unique_id, UniqueId}, G) of
        %% new  swap with heap top
        none ->
            G1 = if HeapTopUniqueId =:= 0 -> G; true-> gb_trees:delete({unique_id, HeapTopUniqueId}, G) end,
            G2 = gb_trees:insert({unique_id, UniqueId}, 1, G1),
            Tuple1 = setelement(1, Tuple, {Key, Value, UniqueId}),
            swap_down(G2, 1, 1, H, Tuple1);
        {value, Index} ->
            ?DBG({Index, Tuple}),
            Cur = element(Index, Tuple),
            {KeyCur, _, _} = Cur, 
            %% bigger than ?
            case KeyCur >= Key of
                true->
                    {H, G, Tuple};
                false->
                    Tuple1 = setelement(Index, Tuple, {Key, Value, UniqueId}),
                    update(G, Index, H, Tuple1)
            end
    end.


swap_down(G, Index, CurH, H, Tuple) when CurH < H ->
    Top = element(Index, Tuple),
    Left = element(2*Index, Tuple),
    Right = element(2*Index+1, Tuple),
    swap_down_1(Top, Left, Right, Index, G, CurH, H, Tuple);

swap_down(G, _, _, H, Tuple)->
    {H, G, Tuple}.

swap_down_1({KeyTop, _, _}, {KeyLeft, _, _}, {KeyRight, _, _}, _, G, _, H, Tuple)
    when KeyTop =< KeyLeft andalso KeyTop =< KeyRight->
        {H, G, Tuple};

swap_down_1({_KeyTop, _, UniqueIdTop} = Top, {KeyLeft, _, _}, {KeyRight, _, UniqueIdRight}=Right, Index, G, CurH, H, Tuple)
    when KeyLeft > KeyRight ->
        Tuple1 = setelement(Index, Tuple, Right),
        Tuple2 = setelement(2*Index+1, Tuple1, Top),
        G1 = enter_unique_id(UniqueIdTop, 2*Index+1, G),
        G2 = enter_unique_id(UniqueIdRight, Index, G1),
        swap_down(G2, 2*Index+1, CurH + 1, H, Tuple2);

swap_down_1({_KeyTop, _, UniqueIdTop} = Top, {_KeyLeft, _, UniqueIdLeft}=Left, {_KeyRight, _, _}, Index, G, CurH, H, Tuple)->
    Tuple1 = setelement(Index, Tuple, Left),
    Tuple2 = setelement(2*Index, Tuple1, Top),
    G1 = gb_trees:enter({unique_id, UniqueIdTop}, 2*Index, G),
    G2 = gb_trees:enter({unique_id, UniqueIdLeft}, Index, G1),
    swap_down(G2, 2*Index, CurH + 1, H, Tuple2).


swap_up(G, 1, H, Tuple)->
    {H, G, Tuple};
swap_up(G, Index, H, Tuple)->
    TopIndex = Index div 2,
    Top = element(TopIndex, Tuple),
    Cur = element(Index, Tuple),
    swap_up_1(G, Cur, Top, Index, TopIndex, H, Tuple).

swap_up_1(G, {KeyCur, _, _}, {KeyTop, _, _}, _, _, H, Tuple) when KeyTop =< KeyCur->
    {H, G, Tuple};

swap_up_1(G, {_, _, UniqueIdCur} = Cur, {_, _, UniqueIdTop} = Top, Index, TopIndex, H, Tuple) ->
    Tuple1 = setelement(TopIndex, Tuple, Cur),
    Tuple2 = setelement(Index, Tuple1, Top),
    G1 = enter_unique_id(UniqueIdCur, TopIndex, G),
    G2 = enter_unique_id(UniqueIdTop, Index, G1),
    swap_up(G2, TopIndex, H, Tuple2).

update(G, Index, H, Tuple)->
    IndexH = get_index_high(Index, 1),
    {H, G1, Tuple1} = swap_up(G, Index, H, Tuple),
    swap_down(G1, Index, IndexH, H, Tuple1).


get_index_high(Index, CurH)->
    case (2 bsl (CurH - 1)) =< Index andalso (Index < (2 bsl CurH)) of
        true ->
            CurH;
        false->
            get_index_high(Index, CurH + 1)
    end.

enter_unique_id(0, _, G)->
    G;
enter_unique_id(UniqueId, Index, G)->
    gb_trees:enter({unique_id, UniqueId}, Index, G).

        





    

