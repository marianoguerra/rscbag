-module(rscbag_server).
-behaviour(gen_server).

-export([start_link/1, get/2, get/3, remove/2, remove_by_val/2, stop/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-record(state, {resource_handler, kv, kv_mod}).

%% API

start_link(Opts) ->
    gen_server:start_link(?MODULE, Opts, []).

get(Bag, Key) ->
    get(Bag, Key, []).

get(Bag, Key, Opts) ->
    gen_server:call(Bag, {get, Key, Opts}).

remove(Bag, Key) ->
    gen_server:call(Bag, {remove, Key}).

remove_by_val(Bag, Val) ->
    gen_server:call(Bag, {remove_by_val, Val}).

stop(Bag) ->
    gen_server:call(Bag, stop).

%% Server implementation, a.k.a.: callbacks

init(Opts) ->
    {resource_handler, ResourceHandler} = proplists:lookup(resource_handler, Opts),
    KvMod = proplists:get_value(kv_mod, Opts, rscbag_ets),
    KvStoreOpts = proplists:get_value(kv_mod_opts, Opts, []),
    {ok, KvStore} = KvMod:init(KvStoreOpts),
    State = #state{resource_handler=ResourceHandler, kv=KvStore, kv_mod=KvMod},
    {ok, State}.

handle_call({get, Key, ROpts}, _From,
            State=#state{kv=Kv, kv_mod=KvMod, resource_handler=RHandler}) ->
    case KvMod:get(Kv, Key) of
        {ok, Val} -> {reply, {ok, found, Val}, State};
        notfound ->
            case RHandler:init(ROpts) of
                {ok, Resource} ->
                    {ok, Kv1} = KvMod:put(Kv, Key, Resource),
                    State1 = State#state{kv=Kv1},
                    {reply, {ok, created, Resource}, State1};
                Other ->
                    {reply, Other, State}
            end
    end;

handle_call({remove, Key}, _From,
            State=#state{kv=Kv, kv_mod=KvMod, resource_handler=RHandler}) ->
    case KvMod:get(Kv, Key) of
        {ok, Val} ->
            RHandler:stop(Val),
            {ok, Kv1} = KvMod:remove(Kv, Key),
            {reply, ok, State#state{kv=Kv1}};
        notfound ->
            {reply, notfound, State}
    end;

handle_call({remove_by_val, Val}, _From,
            State=#state{kv=Kv, kv_mod=KvMod, resource_handler=RHandler}) ->
    case KvMod:remove_by_val(Kv, Val) of
        {ok, Kv1} ->
            RHandler:stop(Val),
            {reply, ok, State#state{kv=Kv1}};
        notfound ->
            {reply, notfound, State}
    end;

handle_call(stop, _From,
            State=#state{kv=Kv, kv_mod=KvMod, resource_handler=RHandler}) ->
    KvMod:foreach(Kv, fun (_Key, Val) ->
                              RHandler:stop(Val)
                      end),
    {stop, normal, stopped, State}.

handle_info(_Msg, State) ->
    %lager:warning("Unexpected handle info message: ~p~n", [Msg]),
    {noreply, State}.

handle_cast(_Msg, State) ->
    %lager:warning("Unexpected handle cast message: ~p~n", [Msg]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
