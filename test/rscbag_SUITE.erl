-module(rscbag_SUITE).
-compile(export_all).

all() -> [noop, get_first_time, get_first_time_ropts_fun, get_second_time,
          get_remove_get, get_remove_by_val_get,
          foldl_empty, foldl_one, foldl_two].


init_per_suite(Config) -> 
    Config.

init_per_testcase(_Test, Config) ->
    EtsOpts = [{resource_handler, test_resource_handler},
               {kv_mod, rscbag_ets}],
    GbOpts = [{resource_handler, test_resource_handler},
               {kv_mod, rscbag_gb_trees}],
    {ok, EtsBag} = rscbag_server:start_link(EtsOpts),
    {ok, GbBag} = rscbag_server:start_link(GbOpts),
    [{ets_bag, EtsBag}, {gb_bag, GbBag}|Config].

end_per_testcase(_Test, Config) ->
    EtsBag = ets_bag(Config),
    GbBag = gb_bag(Config),
    ok = rscbag_server:clean(EtsBag),
    ok = rscbag_server:clean(GbBag),
    Config.

ets_bag(Config) -> proplists:get_value(ets_bag, Config).
gb_bag(Config) -> proplists:get_value(gb_bag, Config).

noop(_) ->
    ok.

get_first_time(Config) ->
    get_first_time_(ets_bag(Config)),
    get_first_time_(gb_bag(Config)).

get_first_time_ropts_fun(Config) ->
    get_first_time_ropts_fun_(ets_bag(Config)),
    get_first_time_ropts_fun_(gb_bag(Config)).

get_first_time_(Bag) ->
    Key = <<"foo">>,
    Opts = [{name, Key}],
    {ok, created, Opts} = rscbag_server:get(Bag, Key, Opts).

get_first_time_ropts_fun_(Bag) ->
    Key = <<"foo">>,
    Opts = [{name, Key}],
    ROpts = fun () -> Opts end,
    {ok, created, Opts} = rscbag_server:get(Bag, Key, ROpts).

get_second_time(Config) ->
    get_second_time_(ets_bag(Config)),
    get_second_time_(gb_bag(Config)).

get_second_time_(Bag) ->
    Key = <<"foo">>,
    Opts = [{name, Key}],
    {ok, created, Opts} = rscbag_server:get(Bag, Key, Opts),
    {ok, found, Opts} = rscbag_server:get(Bag, Key, Opts).

get_remove_get(Config) ->
    get_remove_get_(ets_bag(Config)),
    get_remove_get_(gb_bag(Config)).

get_remove_get_(Bag) ->
    Key = <<"foo">>,
    Opts = [{name, Key}],
    {ok, created, Opts} = rscbag_server:get(Bag, Key, Opts),
    {ok, found, Opts} = rscbag_server:get(Bag, Key, Opts),
    ok = rscbag_server:remove(Bag, Key), 
    {ok, created, Opts} = rscbag_server:get(Bag, Key, Opts).

get_remove_by_val_get(Config) ->
    get_remove_by_val_get_(ets_bag(Config)),
    get_remove_by_val_get_(gb_bag(Config)).

get_remove_by_val_get_(Bag) ->
    Key = <<"foo">>,
    Opts = [{name, Key}],
    Val = Opts,
    {ok, created, Opts} = rscbag_server:get(Bag, Key, Opts),
    {ok, found, Val} = rscbag_server:get(Bag, Key, Opts),
    ok = rscbag_server:remove_by_val(Bag, Val), 
    {ok, created, Val} = rscbag_server:get(Bag, Key, Opts).

foldl_empty(Config) ->
    foldl_empty_(ets_bag(Config)),
    foldl_empty_(gb_bag(Config)).

foldl_count(_Key, _Val, Count) -> Count + 1.
foldl_accum(Key, Val, Accum) -> [{Key, Val}|Accum].

foldl_run(Bag, Items, Fun, State0) ->
    [rscbag_server:get(Bag, Key, Opts) || {Key, Opts} <- Items],
    rscbag_server:foldl(Bag, Fun, State0).

foldl_one(Config) ->
    foldl_one_(ets_bag(Config)),
    foldl_one_(gb_bag(Config)).

foldl_two(Config) ->
    foldl_two_(ets_bag(Config)),
    foldl_two_(gb_bag(Config)).


foldl_empty_(Bag) ->
    {ok, 0} = foldl_run(Bag, [], fun foldl_count/3, 0),
    {ok, []} = foldl_run(Bag, [], fun foldl_accum/3, []).

foldl_one_(Bag) ->
    I1 = {<<"k1">>, [{name, <<"n1">>}]},
    {ok, 1} = foldl_run(Bag, [I1], fun foldl_count/3, 0),
    {ok, [I1]} = foldl_run(Bag, [I1], fun foldl_accum/3, []).

foldl_two_(Bag) ->
    I1 = {<<"k1">>, [{name, <<"n1">>}]},
    I2 = {<<"k2">>, [{name, <<"n2">>}]},
    {ok, 2} = foldl_run(Bag, [I1, I2], fun foldl_count/3, 0),
    {ok, [I2, I1]} = foldl_run(Bag, [I1, I2], fun foldl_accum/3, []).
