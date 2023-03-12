%%%-------------------------------------------------------------------
%% @doc rores top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(rores_sup).

-behaviour(supervisor).

-export([start_link/3]).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link(Port, ValidatorName, ServerName) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [Port, ValidatorName, ServerName]).

init([Port, ValidatorName, ServerName]) ->
    SupFlags = #{strategy => one_for_one,
                 intensity => 0,
                 period => 1},    
    ChildSpecs = [
        {validator, {rores_validator, start_link, [ValidatorName]}, permanent, 5000, worker, [rores_vaildator]},
        {server, {rores_server, start_link, [Port, ValidatorName, ServerName]}, permanent, 5000, worker, [rores_server]}
    ],
    {ok, {SupFlags, ChildSpecs}}.