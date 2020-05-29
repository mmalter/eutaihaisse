%%%-------------------------------------------------------------------
%% @doc eutaihaisse public API
%% @end
%%%-------------------------------------------------------------------

-module(eutaihaisse_app).

-behaviour(application).

-export([start/2, stop/1]).

-include("commons.hrl").

-spec start(_, [table_spec()]) -> {ok, pid()}.
start(_, []) ->
	{ok, SupFlags} = application:get_env(sup_flags),
	{ok, TableSpecs} = application:get_env(table_specifications),
	start([], [SupFlags, TableSpecs]);
start(_, TableSpecs) ->
    eutaihaisse_sup:start_link(TableSpecs).

-spec stop(_) -> ok.
stop(_State) ->
    ok.

%% internal functions
