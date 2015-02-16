-module(rscbag_SUITE).
-compile(export_all).

all() -> [noop, get_first_time, get_second_time, get_remove_get,
          get_remove_by_val_get].


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
    Config.

ets_bag(Config) -> proplists:get_value(ets_bag, Config).
gb_bag(Config) -> proplists:get_value(gb_bag, Config).

noop(_) ->
    ok.

get_first_time(Config) ->
    get_first_time_(ets_bag(Config)),
    get_first_time_(gb_bag(Config)).

get_first_time_(Bag) ->
    Key = <<"foo">>,
    Opts = [{name, Key}],
    {ok, created, Opts} = rscbag_server:get(Bag, Key, Opts).

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
