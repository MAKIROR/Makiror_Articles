%%%-------------------------------------------------------------------
%% @doc rores public API
%% @end
%%%-------------------------------------------------------------------

-module(rores_app).

-behaviour(application).

-export([
     start/2,
     stop/1
     ]).

start(_Type, _StartArgs) ->
    case file:consult("config/server.config") of
        {ok, [[{hostname, Host}, {port, Port}]]} ->
            ValidatorName = list_to_atom(atom_to_list(rores_validator@) ++ atom_to_list(Host)),
            ServerName = list_to_atom(atom_to_list(rores_server@) ++ atom_to_list(Host)),
            rores_sup:start_link(Port, ValidatorName, ServerName);
        Result ->
            ValidatorName = 'rores_validator@127.0.0.1',
            ServerName = 'rores_server@127.0.0.1',
            io:format("Failed to read config file: ~p~n", [Result]),
            rores_sup:start_link(9527, ValidatorName, ServerName)
    end.

stop(_State) ->
    ok.

%% internal functions
