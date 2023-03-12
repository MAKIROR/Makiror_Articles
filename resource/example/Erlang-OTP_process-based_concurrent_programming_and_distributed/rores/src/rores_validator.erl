%%%-------------------------------------------------------------------
%%% @author Makiror
%%% Created : February 2023
%%%-------------------------------------------------------------------

-module(rores_validator).
-behavior(gen_server).

% API
-export([start_link/1, init/1, handle_call/3, handle_cast/2, verify/1]).

start_link(ValidatorName) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [ValidatorName], []).

init([ValidatorName]) ->
    net_kernel:start([ValidatorName]),
    io:format("Validator started: ~p~n", [self()]),
    {ok, []}.

handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

verify(Username) ->
    case length(Username) of
        0 -> 
            {error, "Username cannot be empty."};
        Len when Len >= 6, Len =< 20 -> 
            {ok, Username};
        _ -> 
            {error, "Username must be between 6-20 characters."}
    end.