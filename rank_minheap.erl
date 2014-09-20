%%----------------------------------------------------
%% @desc: minheap for rnaking
%% @author chengcheng<cyoucmi@gmail.com>
%% @date: 2014/9/18
%%----------------------------------------------------

%% MinHeap = { H, list, gb_trees}

-module(rank_minheap).
-export([new/1, insert/4, to_list/1, to_sort_list/1]).

-define(MAX_INDEX(H), (2 bsl (H-1) - 1)).

new(H)->
    G = gb_trees:empty(),
    IndexSize = ?MAX_INDEX(H),
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
    Index = case gb_trees:lookup({unique_id, UniqueId}, G) of
        %% new  swap with heap top
        none ->
            1;
        {value, Index0} ->
            Index0
    end,
    G1 = if Index =:= 1 andalso HeapTopUniqueId =/= 0 -> gb_trees:delete({unique_id, HeapTopUniqueId}, G); true -> G end,
    MaxIndex = ?MAX_INDEX(H),
    Last = element(MaxIndex, Tuple),
    {_, _, UniqueLast} = Last,
    G2 = enter_unique_id(UniqueId, MaxIndex, G1),
    G3 = enter_unique_id(UniqueLast, Index, G2),
    Tuple1 = setelement(MaxIndex, Tuple, {Key, Value, UniqueId}),
    Tuple2 = setelement(Index, Tuple1, Last), 
    IndexH = get_index_high(Index, 1),
    {_, G4, Tuple3} = swap_down(G3, Index, IndexH, H, Tuple2),
    swap_up(G4, MaxIndex, H, Tuple3).


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
    G1 = enter_unique_id(UniqueIdTop, 2*Index, G),
    G2 = enter_unique_id(UniqueIdLeft, Index, G1),
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

get_index_high(Index, CurH)->
    case (2 bsl (CurH - 2)) =< Index andalso (Index < (2 bsl (CurH-1))) of
        true ->
            CurH;
        false->
            get_index_high(Index, CurH + 1)
    end.

enter_unique_id(0, _, G)->
    G;
enter_unique_id(UniqueId, Index, G)->
    gb_trees:enter({unique_id, UniqueId}, Index, G).

to_list(Heap)->
    {_, _, Tuple} = Heap,
    List = tuple_to_list(Tuple),
    lists:filter(
        fun
            ({0, _, _})->
                false;
            (_)->
                true
        end,
        List).

to_sort_list(Heap)->
    List = to_list(Heap),
    lists:sort(
        fun({Key1, _, _}, {Key2, _, _})->
                Key1 > Key2
        end, List).


        





    

