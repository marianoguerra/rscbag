-module(rscbag).

-export([init/1, get/2, get/3, remove/2, remove_by_val/2, clean/1, stop/1]).

-ignore_xref([init/1, get/2, get/3, remove/2, remove_by_val/2]).

-record(state, {resource_handler, kv, kv_mod, kv_opts}).

-type state() :: #state{}.
-type resource_opts() :: term().
-type key() :: term().
-type val() :: term().

-type opts() :: [opt()].
-type opt() ::  {resource_handler, term()} |
                {kv_mod, atom()} |
                {kv_mod_opts, [any()]}.

%% API

-spec init(opts()) -> {ok, state()}.
init(Opts) ->
    {resource_handler, ResourceHandler} = proplists:lookup(resource_handler, Opts),
    KvMod = proplists:get_value(kv_mod, Opts, rscbag_ets),
    KvStoreOpts = proplists:get_value(kv_mod_opts, Opts, []),
    {ok, KvStore} = KvMod:init(KvStoreOpts),
    State = #state{resource_handler=ResourceHandler, kv=KvStore, kv_mod=KvMod,
                   kv_opts=KvStoreOpts},
    {ok, State}.

get(State, Key) ->
    get(State, Key, []).

-spec get(state(), key(), resource_opts()) -> {{ok, found, val()}, state()} |
                                              {{ok, created, val()}, state()} |
                                              {{error, term()}, state()}.
get(State=#state{kv=Kv, kv_mod=KvMod, resource_handler=RHandler}, Key, ROpts) ->
    case KvMod:get(Kv, Key) of
        {ok, Val} -> {{ok, found, Val}, State};
        notfound ->
            case RHandler:init(ROpts) of
                {ok, Resource} ->
                    {ok, Kv1} = KvMod:put(Kv, Key, Resource),
                    State1 = State#state{kv=Kv1},
                    {{ok, created, Resource}, State1};
                Other ->
                    {{error, Other}, State}
            end
    end.

-spec remove(state(), key()) -> {ok, state()} | {{error, notfound}, state()}.
remove(State=#state{kv=Kv, kv_mod=KvMod, resource_handler=RHandler}, Key) ->
    case KvMod:get(Kv, Key) of
        {ok, Val} ->
            RHandler:stop(Val),
            {ok, Kv1} = KvMod:remove(Kv, Key),
            {ok, State#state{kv=Kv1}};
        notfound ->
            {{error, notfound}, State}
    end.

-spec remove_by_val(state(), val()) -> {ok, state()} | {{error, notfound}, state()}.
remove_by_val(State=#state{kv=Kv, kv_mod=KvMod, resource_handler=RHandler}, Val) ->
    case KvMod:remove_by_val(Kv, Val) of
        {ok, Kv1} ->
            RHandler:stop(Val),
            {ok, State#state{kv=Kv1}};
        notfound ->
            {{error, notfound}, State}
    end.

-spec clean(state()) -> {ok, state()}.
clean(State=#state{kv=Kv, kv_mod=KvMod, kv_opts=KvOpts, resource_handler=RHandler}) ->
    KvMod:foreach(Kv, fun (_Key, Val) -> RHandler:stop(Val) end),
    ok = KvMod:stop(Kv),
    {ok, NewKv} = KvMod:init(KvOpts),
    {ok, State#state{kv=NewKv}}.

-spec stop(state()) -> ok.
stop(State=#state{kv=Kv, kv_mod=KvMod, resource_handler=RHandler}) ->
    KvMod:foreach(Kv, fun (_Key, Val) -> RHandler:stop(Val) end),
    clean(State),
    ok.
