-module(eutaihaisse).

-export([create/2, read/2, upsert/2, update/2, delete/2, backup/1, open/1, close/1]).

%public api

create(Table, Objects) ->
    gen_server:call(Table, {create, Objects}).
read(Table, Key) ->
    gen_server:call(Table, {read, Key}).
upsert(Table, Objects) ->
    gen_server:call(Table, {upsert, Objects}).
update(Table, Objects) ->
    gen_server:call(Table, {update, Objects}).
delete(Table, Key) ->
    gen_server:call(Table, {delete, Key}).
backup(Table) ->
    gen_server:call(Table, backup).
open(Table) ->
    gen_server:call(Table, open).
close(Table) ->
    gen_server:call(Table, close).
