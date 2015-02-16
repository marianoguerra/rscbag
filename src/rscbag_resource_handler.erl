-module(rscbag_resource_handler).
-export([behaviour_info/1]).

behaviour_info(callbacks) ->
    [{init, 1}, {stop, 1}];

behaviour_info(_Other) ->
    undefined.

%% copypaste friendly template to start:
%%
%% -export([init/1, stop/1]).
%% -behaviour(rscbag_resource_handler).
%%
%% -type handler() :: TODO
%% -type reason() :: term().

%% -spec init([term()]) -> {ok, handler()} | {error, reason()}.
%% init(Opts) ->
%%   nil.
%%
%% -spec stop(handler()) -> ok | {error, reason()}.
%% stop(Handler) ->
%%   ok. 

