-module(rscbag_store).
-export([behaviour_info/1]).
-export([foldl_fun_to_foreach_fun/1, foldl_fun_to_map_fun/1]).

behaviour_info(callbacks) ->
    [{init, 1},
     {put, 3},
     {get, 2},
     {get, 3},
     {remove, 2},
     {remove_by_val, 2},
     {foldl, 3},
     {foreach, 2},
     {map, 2},
     {stop, 1}];

behaviour_info(_Other) ->
    undefined.

foldl_fun_to_foreach_fun(Fun) ->
    fun (Key, Val, State) ->
            Fun(Key, Val),
            State
    end.

foldl_fun_to_map_fun(Fun) ->
    fun (Key, Val, Accum) ->
            Result = Fun(Key, Val),
            [Result|Accum]
    end.
