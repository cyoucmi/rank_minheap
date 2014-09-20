-module(test).
-export([test/0]).

-define(N, 1000000).
-define(ROLE_ID_MAX, 10000).


test()->
    Heap = rank_minheap:new(6),
    Heap1 = test1(Heap, ?N, 1),
    HeapList = rank_minheap:to_sort_list(Heap1),
    io:format("length(HeapList)=~p~n", [length(HeapList)]),
    io:format("HeapList=~p~n", [HeapList]),
    DictList = get_list(1, []),
    DictList1 = lists:sort(
        fun({Key1, _, _}, {Key2, _, _})->
                Key1 > Key2
        end, DictList),
    DictList2 = lists:sublist(DictList1, 63),
    io:format("DictList2=~p~n", [DictList2]),
    DictList2 =:= HeapList.


test1(Heap, N, N)->
    Heap;

test1(Heap, N, CurN)->
    RoleId = random:uniform(?ROLE_ID_MAX),
    Score = random:uniform(100000000000),
    ItemInfo = random:uniform(1000000),
    Heap1 = rank_minheap:insert(Score, ItemInfo, RoleId, Heap),
    update_dict_info(RoleId, Score, ItemInfo),
    test1(Heap1, N, CurN + 1).

update_dict_info(RoleId, Score, ItemInfo)->
    case erlang:get(RoleId) of
        undefined->
            erlang:put(RoleId, {Score, ItemInfo});
        {CurScore, _} when CurScore >= Score ->
            ok;
        _->
            erlang:put(RoleId, {Score, ItemInfo})
    end.

get_list(N, Acc) when N =< ?ROLE_ID_MAX->
    case erlang:erase(N) of
        undefined ->
            get_list(N+1, Acc);
        {CurScore, CurItemInfo}->
            get_list(N+1, [{CurScore, CurItemInfo, N}|Acc])
    end;

get_list(_, Acc)->
    Acc.

