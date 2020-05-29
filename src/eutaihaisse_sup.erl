%%%-------------------------------------------------------------------
%% @doc eutaihaisse top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(eutaihaisse_sup).

-behaviour(supervisor).

-export([start_link/1]).

-export([init/1]).

-define(SERVER, ?MODULE).
-include("commons.hrl").

-spec start_link(list()) -> {ok, pid()}.
start_link([SupFlags, TableDefinitions]) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [SupFlags, TableDefinitions]).

-spec init(list()) ->
    {ok, {supervisor:sup_flags(), [table_spec()]}}.
init([SupFlags, TableSpec]) ->
    ChildSpecs = [build_table_spec(T) || T <- TableSpec],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
-spec build_table_spec(table_spec()) -> supervisor:child_spec().
build_table_spec({Name, simple, TableDef_}) ->
    Pid_lock = maps:get(pid_lock, TableDef_),
    To_disk = maps:get(to_disk, TableDef_),
    Data_type = maps:get(data_type, TableDef_),
    Keypos = maps:get(keypos, TableDef_),
    TableDef = #simple_table_definition{pid_lock = Pid_lock,
                                        to_disk = To_disk,
                                        data_type = Data_type,
                                        keypos = Keypos
                                       },
    Module = case To_disk of
                 no_persistence -> simple_ets;
                 _ -> simple_dets
             end,
    #{id => Name,
      start => {Module, start_link, [{Name, simple, TableDef}]},
      restart => permanent,
      shutdown => 1000,
      type => worker,
      modules => [gen_server]}.
