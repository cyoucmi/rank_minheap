%%----------------------------------------------------
%% @desc: minheap for rnaking
%% @author chengcheng<cyoucmi@gmail.com>
%% @date: 2014/9/18
%%----------------------------------------------------


%% MinHeap = { H, Tree} 
%% if Tree empty  Tree = nil
%% Tree = {Key, Value, LeftTree, RightTree}

-module(rank_minheap).
-export([new/1, insert/3, to_list/1, to_sort_list/1]).

new(H)->
    {H, make_tree(1, H)}.

make_tree(CurH, H) when CurH < H->
    {0, 0, make_tree(CurH+1, H), make_tree(CurH+1, H)};
make_tree(_, _)->
    {0, 0, nil, nil}.

%% insert key-value
insert(Key, Value, {H, Tree})->
    NewTree = insert_1(Key, Value, Tree),
    {H, NewTree}.

%% new key bigger than min-key
insert_1(Key, Value, {Key1, _, LeftTree, RightTree}) when Key > Key1 ->
    NewTree = {Key, Value, LeftTree, RightTree},
    swap_down(NewTree);

%% new key smaller than min-Key
insert_1(_, _, Tree)->
    Tree.

swap_down({Key, Value, {KeyLeft, _, _, _}=LeftTree, 
        {KeyRight, _, _, _}=RightTree}) when Key =< KeyLeft andalso Key =< KeyRight->
    {Key, Value, LeftTree, RightTree};

swap_down({Key, Value, {KeyLeft, _, _, _}=LeftTree, 
        {KeyRight, ValueRight, LeftTreeRight, RightTreeRight}}) when KeyLeft > KeyRight->
    {KeyRight, ValueRight, LeftTree, swap_down({Key, Value, LeftTreeRight, RightTreeRight})};

swap_down({Key, Value, {KeyLeft, ValueLeft, LeftTreeLeft, RightTreeLeft}, 
        RightTree})->
    {KeyLeft, ValueLeft, swap_down({Key, Value, LeftTreeLeft, RightTreeLeft}), RightTree};

swap_down({Key, Value, nil, nil})->
    {Key, Value, nil, nil}.

to_sort_list(Heap)->
    List = to_list(Heap),
    lists:sort(
        fun({Key1,_}, {Key2, _})->
                Key1<Key2 
        end,
        List).

to_list(Heap)->
    {_, Tree} = Heap,
    to_list_1(Tree).

to_list_1(nil)->
    [];

to_list_1({Key, Value, LeftTree, RightTree})->
    [{Key, Value}|to_list_1(LeftTree) ++ to_list_1(RightTree)].
