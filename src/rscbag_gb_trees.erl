-module(rscbag_gb_trees).
-export([init/1, put/3, get/2, get/3, remove/2, remove_by_val/2, foldl/3,
         foreach/2, map/2, stop/1]).
-behaviour(rscbag_store).

-type store() :: gb_trees:tree().
-type key() :: term().
-type val() :: term().
-type foldl_fun() :: fun((key(), val(), foldl_state()) -> foldl_state()).
-type foldl_state() :: term().
-type map_fun() :: fun((key(), val()) -> term()).
-type foreach_fun() :: fun((key(), val()) -> term()).
-type reason() :: term().

-spec init([term()]) -> {ok, store()} | {error, reason()}.
init(_Opts) ->
    {ok, gb_trees:empty()}.

-spec put(store(), key(), val()) -> {ok, store()} | {error, reason()}.
put(Tree, Key, Val) ->
    Tree1 = gb_trees:insert({key, Key}, Val, Tree),
    Tree2 = gb_trees:insert({val, Val}, Key, Tree1),
    {ok, Tree2}.

-spec get(store(), key()) -> {ok, val()} | notfound.
get(Tree, Key) ->
    ActualKey = {key, Key},
    case gb_trees:lookup(ActualKey, Tree) of
        {value, Val} -> {ok, Val};
        none -> notfound
    end.

-spec get(store(), key(), term()) -> {ok, val()}.
get(Tree, Key, Default) ->
    case get(Tree, Key) of
        {ok, Val} -> {ok, Val};
        notfound -> {ok, Default}
    end.

-spec remove(store(), key()) -> {ok, store()} | notfound.
remove(Tree, Key) ->
    case get(Tree, Key) of
        {ok, Val} ->
            Tree1 = remove_both(Tree, Key, Val),
            {ok, Tree1};
        notfound -> notfound
    end.

-spec remove_by_val(store(), val()) -> {ok, store()} | notfound.
remove_by_val(Tree, Val) ->
    case get_reverse(Tree, Val) of
        {ok, Key} ->
            Tree1 = remove_both(Tree, Key, Val),
            {ok, Tree1};
        notfound ->
            notfound
    end.

-spec foldl(store(), foldl_fun(), foldl_state()) -> {ok, store(), foldl_state()}.
foldl(Tree, Fun, State) ->
    Iter = gb_trees:iterator(Tree),
    Next = gb_trees:next(Iter),
    foldl(Tree, Fun, Next, State).

foldl(Tree, _Fun, none, State) ->
    {ok, Tree, State};

foldl(Tree, Fun, {{val, _ValK}, _Val, Iter}, State) ->
    Next = gb_trees:next(Iter),
    foldl(Tree, Fun, Next, State);

foldl(Tree, Fun, {{key, Key}, Val, Iter}, State) ->
    State1 = Fun(Key, Val, State),
    Next = gb_trees:next(Iter),
    foldl(Tree, Fun, Next, State1).

-spec map(store(), map_fun()) -> {ok, store(), [term()]}.
map(Self, Fun) ->
    WrappedFun = rscbag_store:foldl_fun_to_map_fun(Fun),
    {ok, Self, Result} = foldl(Self, WrappedFun, []),
    {ok, Self, lists:reverse(Result)}.

-spec foreach(store(), foreach_fun()) -> {ok, store()}.
foreach(Self, Fun) ->
    WrappedFun = rscbag_store:foldl_fun_to_foreach_fun(Fun),
    {ok, Self, _Result} = foldl(Self, WrappedFun, ok),
    {ok, Self}.

-spec stop(store()) -> ok.
stop(Tree) ->
    true = ets:delete(Tree),
    ok.

%% private api

remove_both(Tree, Key, Val) ->
    Tree1 = gb_trees:delete_any({val, Val}, Tree),
    Tree2 = gb_trees:delete_any({key, Key}, Tree1),
    Tree2.

-spec get_reverse(store(), val()) -> {ok, key()} | notfound.
get_reverse(Tree, Val) ->
    ActualKey = {val, Val},
    case gb_trees:lookup(ActualKey, Tree) of
        {value, Key} -> {ok, Key};
        none -> notfound
    end.

