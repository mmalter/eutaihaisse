%%%-------------------------------------------------------------------
%% @doc eutaihaisse locker
%% @end
%%%-------------------------------------------------------------------

-module(eutaihaisse_locker).

-behaviour(gen_server).

-export([start_link/1, init/1, handle_call/3, handle_cast/2]).

-define(SERVER, ?MODULE).

-spec start_link(_) -> {ok, pid()}.
start_link(_) ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, []).

-spec init(_) -> {ok, close}.
init(_) ->
	{ok, close}.

-spec handle_call(ask | close | open, pid(), pid()) ->
   	{reply, open | close | busy, close | pid()}.
handle_call({ask, {From, _}}, _, From) ->
	{reply, open, From};
handle_call({ask, _}, _, close) ->
	{reply, close, close};
handle_call(ask, _, Pid) ->
	{reply, busy, Pid};
handle_call({close, {From, _}}, _, From) ->
	{reply, close, close};
handle_call({close, _}, _, Pid) ->
	{reply, busy, Pid};
handle_call({open, {From, _}}, _, close) ->
	{reply, open, From};
handle_call({open, _}, _, Pid) ->
	{reply, busy, Pid}.

handle_cast(_, _) ->
	ok.
