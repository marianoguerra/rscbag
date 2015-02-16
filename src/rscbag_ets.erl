-module(rscbag_ets).
-export([init/1, put/3, get/2, get/3, remove/2, remove_by_val/2, foldl/3,
         foreach/2, map/2, stop/1]).
-behaviour(rscbag_store).

-type store() :: ets:tid().

-type key() :: term().
-type val() :: term().
-type foldl_fun() :: fun((key(), val(), foldl_state()) -> foldl_state()).
-type foldl_state() :: term().
-type map_fun() :: fun((key(), val()) -> term()).
-type foreach_fun() :: fun((key(), val()) -> term()).
-type reason() :: term().

-spec init([term()]) -> {ok, store()} | {error, reason()}.
init(_Opts) ->
    Tid = ets:new(rscbag_ets, [protected, ordered_set]),
    {ok, Tid}.

-spec put(store(), key(), val()) -> {ok, store()} | {error, reason()}.
put(Ref, Key, Val) ->
    ets:insert(Ref, {{key, Key}, Val}),
    ets:insert(Ref, {{val, Val}, Key}),
    {ok, Ref}.

-spec get(store(), key()) -> {ok, val()} | notfound.
get(Ref, Key) ->
    ActualKey = {key, Key},
    case ets:lookup(Ref, ActualKey) of
        [{ActualKey, Val}] -> {ok, Val};
        [] -> notfound
    end.

-spec get(store(), key(), term()) -> {ok, val()}.
get(Ref, Key, Default) ->
    case get(Ref, Key) of
        {ok, Val} -> {ok, Val};
        notfound -> {ok, Default}
    end.

-spec remove(store(), key()) -> {ok, store()} | notfound.
remove(Ref, Key) ->
    case get(Ref, Key) of
        {ok, Val} ->
            remove_both(Ref, Key, Val),
            {ok, Ref};
        notfound -> notfound
    end.

-spec remove_by_val(store(), val()) -> {ok, store()} | notfound.
remove_by_val(Ref, Val) ->
    case get_reverse(Ref, Val) of
        {ok, Key} ->
            remove_both(Ref, Key, Val),
            {ok, Ref};
        notfound ->
            notfound
    end.

-spec foldl(store(), foldl_fun(), foldl_state()) -> {ok, store(), foldl_state()}.
foldl(Ref, Fun, State) ->
    Key = ets:first(Ref),
    foldl(Ref, Fun, Key, State).

foldl(Ref, _Fun, '$end_of_table', State) ->
    {ok, Ref, State};

foldl(Ref, Fun, Key={val, _Val}, State) ->
    NextKey = ets:next(Ref, Key),
    foldl(Ref, Fun, NextKey, State);

foldl(Ref, Fun, WholeKey={key, Key}, State) ->
    {value, Val} = get(Ref, Key),
    State1 = Fun(Key, Val, State),
    NextKey = ets:next(Ref, WholeKey),
    foldl(Ref, Fun, NextKey, State1).

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

-spec stop(store()) -> ok | {error, reason()}.
stop(Ref) ->
    true = ets:delete(Ref),
    ok.

%% private api

remove_both(Ref, Key, Val) ->
    ets:delete(Ref, {val, Val}),
    ets:delete(Ref, {key, Key}).

-spec get_reverse(store(), val()) -> {ok, key()} | notfound.
get_reverse(Ref, Val) ->
    ActualKey = {val, Val},
    case ets:lookup(Ref, ActualKey) of
        [{ActualKey, Key}] -> {ok, Key};
        [] -> notfound
    end.

