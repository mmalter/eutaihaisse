%%%-------------------------------------------------------------------
%% @doc eutaihaisse dets interface
%% @end
%%%-------------------------------------------------------------------

-module(simple_dets).

-behaviour(gen_server).

-export([start_link/1, handle_call/3, handle_cast/2, init/1]).

-include("commons.hrl").

-type table() :: atom().
-type lock() :: pid() | always_open.
-type sd_state() :: {table(), lock(), table_type()}.

%public

-spec start_link(table_spec()) -> {ok, pid()}.
start_link({Name, _, _} = TableSpec) ->
	gen_server:start_link({local, list_to_atom(Name)}, ?MODULE, [TableSpec], []).

-spec init(table_spec()) -> {ok, sd_state()}.
init([TableSpec]) ->
	open_dets_file(TableSpec).

-spec handle_call(table_operations(), pid(), sd_state()) -> {ok, always_open | open | close} | {error, atom()}.
handle_call(open, _, {_, always_open, _} = S) ->
	{reply, always_open, S};
handle_call(open, From, {_, Lock, _} = S) ->
	{reply, gen_server:call(Lock, {open, From}), S};

handle_call(close, From, {_, always_open, _} = S) ->
	{reply, always_open, S};
handle_call(close, From, S = {_, Lock, _}) ->
	{reply, gen_server:call(Lock, {close, From}), S};

handle_call(Request, From, S={Name, Lock, TableDefinition}) ->
	case ask_permission(From, Lock) of
		open -> {reply, handle_authorized_call(Request, Name, TableDefinition), S};
		close -> {reply, close, S}
	end.

-spec handle_authorized_call(create | read | upsert | update | delete | backup,
							 string(),
							 #simple_table_definition{}) ->
   	{rightnow, ok | tuple()}.
handle_authorized_call(backup, Name, TableDefinition) ->
	ok = dets:sync(Name),
	Backup = Name ++ integer_to_list(erlang:monotonic_time()),
	{ok, _} = file:copy(Name, Backup),
	{rightnow, ok};
handle_authorized_call({read, Key}, Name, TableDefinition) ->
	{rightnow, dets:lookup(Name, Key)};
handle_authorized_call(Request, Name, TableDefinition = #simple_table_definition{to_disk=each_write}) ->
	{rightnow, Response} = handle_write(Request, Name, TableDefinition),
	ok = dets:sync(),
	{rightnow, Response};
handle_authorized_call(Request, Name, TableDefinition) ->
	{rightnow, handle_write(Request, Name, TableDefinition)}.

handle_write({create, Objects}, Name, TableDefinition) ->
	dets:insert_new(Name, Objects);
handle_write({upsert, Objects}, Name, TableDefinition) ->
	dets:insert(Name, Objects);
handle_write({update, Objects}, Name, TableDefinition) ->
	dets_update(Name, Objects, TableDefinition = #simple_table_definition.keypos);
handle_write({delete, Key}, Name, TableDefinition) ->
	dets:delete(Name, Key).

-spec dets_update(tuple(), list(), integer()) -> ok | {error, atom()} | {not_in_dets, list()}.
dets_update(Name, Objects, Keypos) when is_list(Objects)->
	Is_not_in_dets = fun (Obj) ->
							 Key = element(Keypos+1, Obj),
							 case dets:member(Name, Key) of
								 true -> false;
								 V -> V
							 end
					 end,
	case lists:filtermap(Is_not_in_dets, Objects) of
		[] -> dets:insert(Name, Objects);
		Keys -> {not_in_dets, Keys}
	end.

handle_cast(_, _) ->
	ok.

%% private functions
-spec open_dets_file(table_spec()) -> {ok, tuple()}.
open_dets_file({Name, simple, TableDefinition}) ->
	{ok, Lock} = case TableDefinition#simple_table_definition.pid_lock of
					true -> gen_server:start_link(eutaihaisse_locker, [], []);
					false -> {ok, always_open}
				end,
	{ok, Name_} = dets:open_file(Name, dets_options(Name, TableDefinition)),
	{ok, {Name_, Lock, TableDefinition}}.

-spec ask_permission(pid(), always_open | pid()) -> open | close | busy.
ask_permission(_, always_open) ->
	open;
ask_permission(From, Lock) ->
	gen_server:call(Lock, {ask, From}).

-spec dets_options(table_name(), #simple_table_definition{}) -> list().
dets_options(Name, TableDefinition) ->
	Td = TableDefinition#simple_table_definition.to_disk,
	Type = TableDefinition#simple_table_definition.data_type,
	Kp = TableDefinition#simple_table_definition.keypos,
	[{type, Type},
	 {file, ?DB ++ Name ++ ".dets"},
	 {access, read_write},
	 {auto_save, dets_auto_save(Td)},
	 {repair, true},
	 {ram_file, false},
	 {keypos, Kp}
	].

-spec dets_auto_save(to_disk()) -> infinity | integer().
dets_auto_save(only_backups) ->
	infinity;
dets_auto_save(each_write) ->
	infinity;
dets_auto_save(no_persistence) ->
	infinity;
dets_auto_save(Int) ->
	Int.
