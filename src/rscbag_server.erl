-module(rscbag_server).
-behaviour(gen_server).

-export([start_link/1, get/2, get/3, remove/2, remove_by_val/2, clean/1,
         stop/1, foldl/3]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-ignore_xref([start_link/1, get/2, get/3, remove/2, remove_by_val/2, clean/1, stop/1]).

%% API

start_link(Opts) ->
    gen_server:start_link(?MODULE, Opts, []).

get(Bag, Key) ->
    get(Bag, Key, []).

get(Bag, Key, Opts) ->
    gen_server:call(Bag, {get, Key, Opts}).

remove(Bag, Key) ->
    gen_server:call(Bag, {remove, Key}).

foldl(Bag, Fun, State0) ->
    gen_server:call(Bag, {foldl, Fun, State0}).

remove_by_val(Bag, Val) ->
    gen_server:call(Bag, {remove_by_val, Val}).

stop(Bag) ->
    gen_server:call(Bag, stop).

clean(Bag) ->
    gen_server:call(Bag, clean).

%% Server implementation, a.k.a.: callbacks

init(Opts) ->
    rscbag:init(Opts).

handle_call({get, Key, ROpts}, _From, State) ->
    {Reply, NewState} = rscbag:get(State, Key, ROpts),
    {reply, Reply, NewState};

handle_call({remove, Key}, _From, State) ->
    {Reply, NewState} = rscbag:remove(State, Key),
    {reply, Reply, NewState};

handle_call({remove_by_val, Val}, _From, State) ->
    {Reply, NewState} = rscbag:remove_by_val(State, Val),
    {reply, Reply, NewState};

handle_call({foldl, Fun, State0}, _From, State) ->
    {Status, NewState, Result} = rscbag:foldl(State, Fun, State0),
    {reply, {Status, Result}, NewState};

handle_call(clean, _From, State) ->
    {Reply, NewState} = rscbag:clean(State),
    {reply, Reply, NewState};

handle_call(stop, _From, State) ->
    ok = rscbag:stop(State),
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
