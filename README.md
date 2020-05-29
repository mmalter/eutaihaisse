# eutaihaisse

Some datastores implemented in Erlang, with a consistent interface.

## Use cases
- I want to put stuff somewhere, have it readily available in memory but I don't need it to be distributed across multiple nodes. Keep the damn thing simple and available on BEAM.
- I don't want mnesia.
- I want dirtier writes than mnesia.
- I want to write my own datastore but I want a simple basis on which I can get started. This is the principal intent.
- I am fucking bored of copying a single access ETS module every day of my life.

Some friendly advices:
- Keep your problem small.
- Learn ets, dets and mnesia. Use epocxy for more extreme use cases.

## General notes
- Tables are persisted in priv/ with a timestamp value of the dump. Don't keep too many backups on your production machine. You could develop an application to retrieve these backups and send these where they belong.
- Locking a table to a pid can help you deal with concurrency.

## Usage
Your tables are specified in the application configuration file. You can pass supervisor flags and table specifications. Each caching method has different options but things are kept consistent where possible. Alternatively, you can simply pass the supervisor flags and the table specifications to the start function.

## Caching methods
Well, so far there is only one. Come back later for more!

### simple
#### Description
Just a gen_server that owns an ETS/DETS table. Each table has a 2GB limit.

#### Options
Lock the table to a specific process open/1 and close/1.

    pid_lock :: true | false

You can choose to persist repectively, only through external call of table_name:backup/0, after each write, periodically or never.

    to_disk :: only_backups | each_write | integer() | no_persistence

A set is a tuple with a key and an object. A bag has many objects but only one instance of each object, per key. A duplicate_bag can have many objects, including multiple copies of the same object, per key.

    data_type :: set | bag | duplicate_bag

The position of the key in your objects.

    keypos :: integer

## Build

    $ rebar3 compile
