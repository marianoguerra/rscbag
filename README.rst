Resource Bag
============

An Erlang library to hold a bag of resources, get them by key and initialize
them and store them on get if not found.

The store and the resouce life cycle can be parameterized, we provide a default
store using ets (default) and another one using gb_trees for your convenience.

Concepts
--------

Store
.....

a simple key value store that holds for each key a reference to a resource,
it can put/get/remove and remove_by_value.

Resource Handler
................

a simple behaviour that provides logic to init and stop your resource, you have
to provide this since we don't know what kind of resources you want to handle.

see test/test_resource_handler.erl for a simple example that initializes a
resource that is simply the Opts passed to it during init.

Usage
-----

You have to have a module that manages your resources, it must implement two
functions::

    init(opts()) -> {ok, state()} | {error, reason()}.
    stop(state()) -> ok | {error, reason()}.

then create an instance of rscbag::

    {ok, Bag} = rscbag_server:start_link(Opts),

where Opts is a proplist with one required key and one optional one:

* resource_handler (Required): an atom with the name of the module that implements init/1, stop/1
* kv_mod (Optional, defaults to rscbag_ets): an atom with the name of the
  module that will be used as kv store for the resources, there are two implemented
  in this library, rscbag_ets and rscbag_gb_trees, you can implement your own
  by implemented the rscbag_store behaviour.

then you can use the rscbag API to get, get_existing, remove, remove_by_val, clean, stop etc.

License
-------

MPL 2.0, see LICENSE

