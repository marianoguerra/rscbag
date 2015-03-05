-module(rscbag).

-export([init/1, get/2, get/3, remove/2, remove_by_val/2]).

-ignore_xref([init/1, get/2, get/3, remove/2, remove_by_val/2]).

-record(state, {resource_handler, kv, kv_mod}).

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
    State = #state{resource_handler=ResourceHandler, kv=KvStore, kv_mod=KvMod},
    {ok, State}.

get(State, Key) ->
    get(State, Key, []).

get(State=#state{kv=Kv, kv_mod=KvMod, resource_handler=RHandler}, Key, ROpts) ->
    case KvMod:get(Kv, Key) of
        {ok, Val} -> {reply, {ok, found, Val}, State};
        notfound ->
            case RHandler:init(ROpts) of
                {ok, Resource} ->
                    {ok, Kv1} = KvMod:put(Kv, Key, Resource),
                    State1 = State#state{kv=Kv1},
                    {{ok, created, Resource}, State1};
                Other ->
                    {error, Other, State}
            end
    end.

-spec remove(state(), key()) -> {ok, state()} | {error, notfound, state()}.
remove(State=#state{kv=Kv, kv_mod=KvMod, resource_handler=RHandler}, Key) ->
    case KvMod:get(Kv, Key) of
        {ok, Val} ->
            RHandler:stop(Val),
            {ok, Kv1} = KvMod:remove(Kv, Key),
            {ok, State#state{kv=Kv1}};
        notfound ->
            {error, notfound, State}
    end.

-spec remove_by_val(state(), val()) -> {ok, state()} | {error, notfound, state()}.
remove_by_val(State=#state{kv=Kv, kv_mod=KvMod, resource_handler=RHandler}, Val) ->
    case KvMod:remove_by_val(Kv, Val) of
        {ok, Kv1} ->
            RHandler:stop(Val),
            {ok, State#state{kv=Kv1}};
        notfound ->
            {error, notfound, State}
    end.
