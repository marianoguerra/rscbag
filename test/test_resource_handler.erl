-module(test_resource_handler).
-export([init/1, stop/1]).

-behaviour(rscbag_resource_handler).

-type handler() :: term().
-type reason() :: term().
-type opts() :: [term()].

-spec init(opts()) -> {ok, opts()} | {error, reason()}.
init(Opts) ->
    {ok, Opts}.

-spec stop(handler()) -> ok | {error, reason()}.
stop(_Handler) ->
  ok. 

