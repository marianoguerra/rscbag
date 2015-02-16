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

License
-------

MPL 2.0, see LICENSE

